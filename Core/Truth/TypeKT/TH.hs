{-# OPTIONS -fno-warn-unused-binds -w #-}
module Truth.TypeKT.TH where
{
    import Language.Haskell.TH;
    import Control.Monad;
    import Prelude(Show(..),Eq(..),Num(..),String);
    import qualified Data.Witness;
    import Data.List;
    import Data.Char;

    type1 :: TypeQ -> ExpQ;
    type1 t = [|Data.Witness.Type :: Data.Witness.Type ($(t) ())|];

    typeBndrDeclKind :: [TyVarBndr] -> Q Kind;
    typeBndrDeclKind [] = return StarK;
    typeBndrDeclKind (KindedTV _ k1:bs) = do
    {
        k2 <- typeBndrDeclKind bs;
        return (ArrowK k1 k2);
    };
    typeBndrDeclKind (PlainTV _n:bs) = do
    {
        -- report False ("assuming kind * for tyvar " ++ (show n));
        k2 <- typeBndrDeclKind bs;
        return (ArrowK StarK k2);
    };

    decKind :: Dec -> Q Kind;
    decKind (DataD _ _ tvs _ _) = typeBndrDeclKind tvs;
    decKind (NewtypeD _ _ tvs _ _) = typeBndrDeclKind tvs;
    decKind (TySynD _ _ t) = thTypeKind t;
    decKind dec = fail ("can't figure out kind of decl " ++ (show dec));

    thTypeKind :: Type -> Q Kind;
    thTypeKind (ForallT _ _ t) = thTypeKind t;
    thTypeKind (VarT name) = fail ("can't figure out kind of tyvar " ++ (show name));
    thTypeKind (ConT name) = do
    {
        info <- reify name;
        case info of
        {
            TyConI dec -> decKind dec;
            (PrimTyConI _ n _) -> thTypeKind (TupleT n); -- best guess
            _ -> fail ("can't figure out kind of info " ++ (show info));
        };
    };
    thTypeKind (TupleT n) = return (tupleK n) where
    {
        tupleK 0 = StarK;
        tupleK i = ArrowK StarK (tupleK (i - 1));
    };
    thTypeKind ArrowT = return (ArrowK StarK (ArrowK StarK StarK));
    thTypeKind ListT = return (ArrowK StarK StarK);
    thTypeKind (AppT t _) = do
    {
        k <- thTypeKind t;
        case k of
        {
            ArrowK _ k' -> return k';
            StarK -> fail "bad AppT";
        };
    };
    thTypeKind (SigT _ k) = return k;

    supportedKinds :: [Kind];
    supportedKinds =
    [
        StarK,
        ArrowK StarK StarK,
        ArrowK StarK (ArrowK StarK StarK),
        ArrowK StarK (ArrowK StarK (ArrowK StarK StarK)),
        ArrowK (ArrowK StarK StarK) StarK,
        ArrowK (ArrowK StarK StarK) (ArrowK StarK StarK),
        ArrowK (ArrowK StarK StarK) (ArrowK StarK (ArrowK StarK StarK)),
        ArrowK (ArrowK StarK StarK) (ArrowK (ArrowK StarK StarK) (ArrowK StarK StarK))
    ];

    kindCode :: Kind -> String;
    kindCode StarK = "T";
    kindCode (ArrowK a b) = "K" ++ (kindCode a) ++ (kindCode b);

    kTypeTypeName :: Kind -> Name;
    kTypeTypeName k = mkName ("Type_" ++ (kindCode k));

    kTypeType :: Kind -> Type;
    kTypeType k = ConT (kTypeTypeName k);

    kTypeTypeQ :: Kind -> TypeQ;
    kTypeTypeQ k = return (kTypeType k);

    kKindTypeName :: Kind -> Name;
    kKindTypeName k = mkName ("Kind_" ++ (kindCode k));

    kKindType :: Kind -> Type;
    kKindType k = ConT (kKindTypeName k);

    kKindConsName :: Kind -> Name;
    kKindConsName k = mkName ("Kind_" ++ (kindCode k));
}

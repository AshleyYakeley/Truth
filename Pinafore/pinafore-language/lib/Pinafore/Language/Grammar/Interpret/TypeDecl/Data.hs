module Pinafore.Language.Grammar.Interpret.TypeDecl.Data
    ( makeDataTypeBox
    ) where

import qualified Data.List as List
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

-- | Structure of a datatype
data PinaforeDataType :: forall (k :: Type). k -> Type where
    NilDataType :: PinaforeDataType Void
    ConsDataType
        :: ListType (PinaforeNonpolarType '[]) tl -> PinaforeDataType tt -> PinaforeDataType (Either (HList tl) tt)

-- | Structural equality
instance TestHetEquality PinaforeDataType where
    testHetEquality NilDataType NilDataType = return HRefl
    testHetEquality (ConsDataType a1 ar) (ConsDataType b1 br) = do
        Refl <- testEquality a1 b1
        HRefl <- testHetEquality ar br
        return HRefl
    testHetEquality _ _ = Nothing

data DataBox =
    forall t. MkDataBox (PinaforeDataType t)
                        [Constructor (PinaforeNonpolarType '[]) t]

assembleDataType :: [(Name, AnyW (ListType (PinaforeNonpolarType '[])))] -> DataBox
assembleDataType [] = MkDataBox NilDataType []
assembleDataType ((n, MkAnyW el):cc) =
    case assembleDataType cc of
        MkDataBox ct conss ->
            MkDataBox (ConsDataType el ct) $ (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

datatypeIOWitness :: IOWitness ('MkWitKind IdentifiedFamily)
datatypeIOWitness = $(iowitness [t|'MkWitKind IdentifiedFamily|])

interpretDataTypeConstructor ::
       SyntaxDatatypeConstructor -> PinaforeInterpreter (Name, AnyW (ListType (PinaforeNonpolarType '[])))
interpretDataTypeConstructor (MkSyntaxDatatypeConstructor consName stypes) = do
    etypes <- for stypes interpretNonpolarType
    return (consName, assembleListType etypes)

makeDataTypeBox ::
       Name
    -> Markdown
    -> [SyntaxDatatypeParameter]
    -> [SyntaxDatatypeConstructor]
    -> PinaforeInterpreter PinaforeTypeBox
makeDataTypeBox name doc [] sconss = do
    tid <- newTypeID
    return $
        valueToWitness tid $ \tidsym -> let
            gt = singleGroundType' @'[] (MkFamilyType datatypeIOWitness $ MkIdentifiedFamily tidsym) $ exprShowPrec name
            mktype _ = MkBoundType gt
            in mkTypeFixBox name doc mktype $ do
                   tconss <- for sconss interpretDataTypeConstructor
                   MkDataBox _dt conss <- return $ assembleDataType tconss
                   let
                       freevars :: [AnyW SymbolType]
                       freevars = nub $ mconcat $ fmap constructorFreeVariables conss
                       declaredvars :: [AnyW SymbolType]
                       declaredvars = [] -- ISSUE #41
                       unboundvars :: [AnyW SymbolType]
                       unboundvars = freevars List.\\ declaredvars
                   case nonEmpty unboundvars of
                       Nothing -> return ()
                       Just vv ->
                           throw $ InterpretUnboundTypeVariablesError $ fmap (\(MkAnyW s) -> symbolTypeToName s) vv
                   let
                       ctf :: forall polarity. Is PolarityType polarity
                           => PinaforeShimWit polarity _
                       ctf = singleDolanShimWit $ mkPolarShimWit $ GroundedDolanSingularType gt NilDolanArguments
                   tident <- unsafeIdentify
                   let tiso = reflId tident
                   patts <-
                       for conss $ \(MkConstructor cname lt at tma) -> do
                           ltp <- return $ mapListType nonpolarToDolanType lt
                           ltn <- return $ mapListType nonpolarToDolanType lt
                           let
                               expr =
                                   qConstExprAny $
                                   MkAnyValue (qFunctionPosWitnesses ltn ctf) $ \hl -> isoBackwards tiso $ at hl
                               pc = toPatternConstructor ctf ltp $ \t -> tma $ isoForwards tiso t
                           withNewPatternConstructor cname doc expr pc
                   return ((), compAll patts)
makeDataTypeBox _name _doc _ _sconss = throw $ UnicodeDecodeError "ISSUE #41"

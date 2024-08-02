module Pinafore.Language.Interpret.TypeDecl.Predicate
    ( makePredicateTypeBox
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Types
import Pinafore.Language.Type

predicateGroundType ::
       forall tid (t :: Type). (IdentifiedKind tid ~ Type, Identified tid ~~ t)
    => FullName
    -> TypeIDType tid
    -> QGroundedShimWit 'Negative t
    -> QOpenExpression (t -> Bool)
    -> QGroundType '[] t
predicateGroundType name tid parentneg _predexpr = let
    props = mempty
        -- singleGroundProperty storabilityProperty $ dynamicEntityStorability $ fmap Just $ getTypeSet fam
    in (singleGroundType' (identifiedFamilialType tid) props $ exprShowPrec name)
           { qgtGreatestDynamicSupertype =
                 MkPolyGreatestDynamicSupertype $ \NilCCRArguments ->
                     getGroundedShimWitGreatestDynamicSupertype parentneg
           }

makePredicateTypeBox ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => FullName
    -> RawMarkdown
    -> Bool
    -> SyntaxType
    -> SyntaxExpression
    -> QInterpreter (QFixBox () ())
makePredicateTypeBox name md storable sparent spredicate =
    withNewTypeID $ \(tidsym :: TypeIDType tid) -> do
        Refl <- unsafeIdentifyKind @tid @Type tidsym
        let
            register ::
                   (QNonpolarGroundedType (Identified tid), QOpenExpression (Identified tid -> Bool))
                -> QScopeBuilder ()
            register ~(parent, oexpr) = do
                let
                    parentneg :: QGroundedShimWit 'Negative (Identified tid)
                    parentneg = groundedNonpolarToDolanType parent
                    gt :: QGroundType '[] (Identified tid)
                    gt = predicateGroundType name tidsym parentneg oexpr
                    gdsname = exprShow $ getGroundedShimWitGreatestDynamicSupertype parentneg
                    doc = MkDefDoc (typeDocItem name storable [] (Just gdsname)) md
                    sce :: QSubtypeConversionEntry
                    sce = neutralSubtypeConversionEntry (MkNonpolarGroundedType gt NilCCRArguments) parent
                registerGroundType name doc gt
                registerSubtypeConversion sce
            construct ::
                   ()
                -> QScopeBuilder ( (QNonpolarGroundedType (Identified tid), QOpenExpression (Identified tid -> Bool))
                                 , ())
            construct () =
                builderLift $ do
                    smparent <- interpretNonpolarGroundedType sparent
                    case smparent of
                        MkSome (parent :: _ tparent) -> do
                            Refl <- unsafeIdentify @_ @tparent tidsym
                            let
                                parentneg :: PShimWit QIsoShim QGroundedType 'Negative (Identified tid)
                                parentneg = groundedNonpolarToDolanType parent
                            expr <- ?interpretExpression spredicate
                            oexpr <- qSubsumeExpressionToOpenWit (funcShimWit (shimWitToDolan parentneg) qType) expr
                            return ((parent, oexpr), ())
        return $ mkFixBox register construct

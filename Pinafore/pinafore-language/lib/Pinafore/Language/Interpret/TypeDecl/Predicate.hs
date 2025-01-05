module Pinafore.Language.Interpret.TypeDecl.Predicate
    ( makePredicateTypeBox
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpret.TypeDecl.Storage
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Types
import Pinafore.Language.Type

gate :: (t -> Bool) -> Maybe t -> Maybe t
gate f (Just t)
    | f t = Just t
gate _ _ = Nothing

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
            register ~(parent, predexpr) = do
                let
                    freevars :: ListSet SomeTypeVarT
                    freevars = freeTypeVariables parent
                    declaredvars :: [SomeTypeVarT]
                    declaredvars = mempty -- TBD
                    unboundvars :: [SomeTypeVarT]
                    unboundvars = toList freevars \\ declaredvars
                case nonEmpty unboundvars of
                    Nothing -> return ()
                    Just vv ->
                        builderLift $
                        throw $ InterpretTypeDeclUnboundTypeVariablesError name $ fmap someTypeVarToName vv
                let
                    parentneg :: QGroundedShimWit 'Negative (Identified tid)
                    parentneg = groundedNonpolarToDolanType parent
                    gds :: QExprGroundedShimWit 'Negative (Maybe (Identified tid))
                    gds = getGroundedShimWitGreatestDynamicSupertype parentneg
                props :: GroundProperties '[] (Identified tid) <-
                    if storable
                        then do
                            storeadapterexpr <- builderLift $ nonpolarGroundedToStoreAdapter NilCCRArguments parent
                            wit <- liftIO newIOWitness
                            let
                                storability :: Storability '[] (Identified tid)
                                storability = let
                                    stbKind = NilListType
                                    stbCovaryMap = covarymap
                                    stbAdapterExprKnot =
                                        knotAppRec wit $
                                        liftA2
                                            (\prd (Compose adap) ->
                                                 MkAllFor $ \NilArguments -> gateStoreAdapter prd $ adap NilArguments)
                                            (liftAppRec predexpr)
                                            storeadapterexpr
                                    in MkStorability {..}
                            return $ singleGroundProperty storabilityProperty storability
                        else return mempty
                let
                    gt :: QGroundType '[] (Identified tid)
                    gt =
                        (singleGroundType' (identifiedFamilialType tidsym) props $ exprShowPrec name)
                            { qgtGreatestDynamicSupertype =
                                  MkPolyGreatestDynamicSupertype $ \NilCCRArguments ->
                                      case gds of
                                          MkShimWit dpt (MkPolarShim (MkComposeShim convexpr)) ->
                                              MkShimWit dpt $
                                              MkPolarShim $
                                              MkComposeShim $
                                              liftA2
                                                  (\prd conv -> (functionToShim "predicate" $ gate prd) . conv)
                                                  predexpr
                                                  convexpr
                            }
                    gdsname = exprShow gds
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

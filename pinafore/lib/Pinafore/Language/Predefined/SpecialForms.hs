module Pinafore.Language.Predefined.SpecialForms
    ( special_forms
    ) where

import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Error
import Pinafore.Language.Interpret.TypeDecl
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Scope
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value
import Shapes

special_forms :: [DocTreeEntry BindDoc]
special_forms =
    [ docTreeEntry
          "Special Forms"
          "These are built-in keywords that resemble predefined bindings."
          [ mkSpecialFormEntry
                "property"
                "A property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
                "@A @B <anchor>"
                "A ~> B" $
            MkSpecialForm
                (ConsListType AnnotConcreteEntityType $
                 ConsListType AnnotConcreteEntityType $ ConsListType AnnotAnchor NilListType) $ \(MkAnyW eta, (MkAnyW etb, (anchor, ()))) -> do
                etan <- concreteEntityToNegativePinaforeType eta
                etbn <- concreteEntityToNegativePinaforeType etb
                let
                    bta = biRangeAnyF (etan, concreteToPositiveDolanType eta)
                    btb = biRangeAnyF (etbn, concreteToPositiveDolanType etb)
                    in case (bta, btb, concreteEntityTypeEq eta, concreteEntityTypeEq etb) of
                           (MkAnyF rta (MkRange praContra praCo), MkAnyF rtb (MkRange prbContra prbCo), Dict, Dict) ->
                               withSubrepresentative rangeTypeInKind rta $
                               withSubrepresentative rangeTypeInKind rtb $ let
                                   typef =
                                       singleDolanShimWit $
                                       mkShimWit $
                                       GroundDolanSingularType MorphismPinaforeGroundType $
                                       ConsDolanArguments rta $ ConsDolanArguments rtb NilDolanArguments
                                   morphism =
                                       propertyMorphism
                                           (concreteEntityAdapter eta)
                                           (concreteEntityAdapter etb)
                                           (MkPredicate anchor)
                                   pinamorphism =
                                       MkLangMorphism $
                                       cfmap3 (MkCatDual $ shimToFunction praContra) $
                                       cfmap2 (shimToFunction praCo) $
                                       cfmap1 (MkCatDual $ shimToFunction prbContra) $
                                       fmap (shimToFunction prbCo) morphism
                                   anyval = MkAnyValue typef pinamorphism
                                   in return anyval
          , mkSpecialFormEntry
                "openEntity"
                "An open entity for this anchor. `A` is an open entity type."
                "@A <anchor>"
                "A" $
            MkSpecialForm (ConsListType AnnotConcreteEntityType $ ConsListType AnnotAnchor NilListType) $ \(MkAnyW tp, (anchor, ())) -> do
                pt <- makeEntity tp $ MkEntity anchor
                let typef = concreteToPositiveDolanType tp
                return $ MkAnyValue typef pt
          , mkSpecialFormEntry "newOpenEntity" "Generate an open entity. `A` is an open entity type." "@A" "Action A" $
            MkSpecialForm (ConsListType AnnotOpenEntityType NilListType) $ \(MkAnyW (tp :: OpenEntityType tid), ()) -> do
                let
                    pt :: PinaforeAction (OpenEntity tid)
                    pt = liftIO $ newKeyContainerItem @(FiniteSet (OpenEntity tid))
                    typef =
                        actionShimWit $
                        singleDolanShimWit $
                        mkShimWit $
                        GroundDolanSingularType
                            (EntityPinaforeGroundType NilListType $ OpenEntityGroundType tp)
                            NilDolanArguments
                return $ MkAnyValue typef pt
          , mkSpecialFormEntry
                "evaluate"
                "A function that evaluates text as a Pinafore expression to be subsumed to positive type `A`.\n\
                \The result of the action is either the value (`Right`), or an error message (`Left`).\n\
                \The local scope is not in any way transmitted to the evaluation."
                "@A"
                "Text -> Action (Either Text A)" $
            MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(MkAnyW tp, ()) -> do
                spvals <- getSpecialVals
                let
                    eitherShimWit ::
                           forall a b.
                           PinaforeShimWit 'Positive a
                        -> PinaforeShimWit 'Positive b
                        -> PinaforeShimWit 'Positive (Either a b)
                    eitherShimWit swa swb =
                        unPosShimWit swa $ \ta conva ->
                            unPosShimWit swb $ \tb convb ->
                                mapPosShimWit (applyCoPolyShim (cfmap conva) convb) $
                                singleDolanShimWit $
                                mkShimWit $
                                GroundDolanSingularType
                                    (EntityPinaforeGroundType
                                         (ConsListType Refl $ ConsListType Refl NilListType)
                                         EitherEntityGroundType) $
                                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
                    funcShimWit ::
                           forall a b.
                           PinaforeShimWit 'Negative a
                        -> PinaforeShimWit 'Positive b
                        -> PinaforeShimWit 'Positive (a -> b)
                    funcShimWit swa swb =
                        unNegShimWit swa $ \ta conva ->
                            unPosShimWit swb $ \tb convb ->
                                mapPosShimWit (applyCoPolyShim (ccontramap conva) convb) $
                                singleDolanShimWit $
                                mkShimWit $
                                GroundDolanSingularType FuncPinaforeGroundType $
                                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
                    textShimWit ::
                           forall polarity. Is PolarityType polarity
                        => PinaforeShimWit polarity Text
                    textShimWit =
                        singleDolanShimWit $
                        mkShimWit $
                        GroundDolanSingularType
                            (EntityPinaforeGroundType NilListType $ LiteralEntityGroundType TextLiteralType)
                            NilDolanArguments
                    valShimWit ::
                           forall t.
                           PinaforeShimWit 'Positive t
                        -> PinaforeShimWit 'Positive (Text -> PinaforeAction (Either Text t))
                    valShimWit t' = funcShimWit textShimWit $ actionShimWit $ eitherShimWit textShimWit t'
                return $ MkAnyValue (valShimWit $ mkShimWit tp) $ specialEvaluate spvals tp
          ]
    ]

actionShimWit :: forall a. PinaforeShimWit 'Positive a -> PinaforeShimWit 'Positive (PinaforeAction a)
actionShimWit swa =
    unPosShimWit swa $ \ta conva ->
        mapPosShimWit (cfmap conva) $
        singleDolanShimWit $
        mkShimWit $ GroundDolanSingularType ActionPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

makeEntity :: MonadThrow ErrorType m => ConcreteEntityType t -> Entity -> m t
makeEntity (MkConcreteType TopEntityGroundType NilArguments) p = return p
makeEntity (MkConcreteType (OpenEntityGroundType _) NilArguments) p = return $ MkOpenEntity p
makeEntity t _ = throw $ InterpretTypeNotOpenEntityError $ exprShow t

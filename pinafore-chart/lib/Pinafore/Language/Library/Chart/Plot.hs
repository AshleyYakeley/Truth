{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Chart.Plot where

import Data.Shim
import Graphics.Rendering.Chart as C
import Language.Expression.Dolan
import Pinafore.Language.API
import Shapes

type LangPlot :: Type -> Type -> Type
newtype LangPlot x y =
    MkLangPlot (Plot x y)

instance Functor Limit where
    fmap _ LMin = LMin
    fmap _ LMax = LMax
    fmap ab (LValue a) = LValue $ ab a

instance Functor (LangPlot a) where
    fmap ab (MkLangPlot (Plot r l (xx, yy))) =
        MkLangPlot $ Plot (\pmf -> r $ \(lx, ly) -> pmf $ (lx, fmap ab ly)) l (xx, fmap ab yy)

instance RepresentationalRole (LangPlot a) where
    representationalCoercion MkCoercion = MkCoercion

instance HasVariance 'Covariance (LangPlot a) where
    varianceRepresentational = Just Dict

instance CatFunctor (->) (NestedMorphism (->)) LangPlot where
    cfmap ab =
        MkNestedMorphism $ \(MkLangPlot (Plot r l (xx, yy))) ->
            MkLangPlot $ Plot (\pmf -> r $ \(lx, ly) -> pmf $ (fmap ab lx, ly)) l (fmap ab xx, yy)

instance RepresentationalRole LangPlot where
    representationalCoercion MkCoercion = MkCoercion

instance HasVariance 'Covariance LangPlot where
    varianceRepresentational = Just Dict

plotGroundType :: PinaforeGroundType '[ 'Covariance, 'Covariance] LangPlot
plotGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangPlot)|]) "Plot"

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) (LangPlot a b) where
    toShimWit =
        unPosShimWit toJMShimWit $ \ta conva ->
            unPosShimWit toJMShimWit $ \tb convb ->
                mapPosShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType plotGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) a
         , ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) b
         ) => ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) (LangPlot a b) where
    toShimWit = singleDolanShimWit toJMShimWit

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) (LangPlot a b) where
    fromShimWit =
        unNegShimWit fromJMShimWit $ \ta conva ->
            unNegShimWit fromJMShimWit $ \tb convb ->
                mapNegShimWit (applyCoPolyShim (cfmap conva) convb) $
                mkShimWit $
                GroundDolanSingularType plotGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) a
         , FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) b
         ) => FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) (LangPlot a b) where
    fromShimWit = singleDolanShimWit fromJMShimWit

pointStyleGroundType :: PinaforeGroundType '[] PointStyle
pointStyleGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual PointStyle)|]) "PointStyle"

-- PointStyle
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) PointStyle where
    toShimWit = mkShimWit $ GroundDolanSingularType pointStyleGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) PointStyle where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) PointStyle where
    fromShimWit = mkShimWit $ GroundDolanSingularType pointStyleGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) PointStyle where
    fromShimWit = singleDolanShimWit fromJMShimWit

plotPoints :: Text -> PointStyle -> [(X, Y)] -> LangPlot X Y
plotPoints title style points = MkLangPlot $ toPlot $ PlotPoints (unpack title) style points

langJoinPlot :: LangPlot X Y -> LangPlot X Y -> LangPlot X Y
langJoinPlot (MkLangPlot p) (MkLangPlot q) = MkLangPlot $ joinPlot p q

plotHidden :: [X] -> [Y] -> LangPlot X Y
plotHidden xx yy = MkLangPlot $ toPlot $ PlotHidden xx yy

plotStuff :: DocTreeEntry BindDoc
plotStuff =
    docTreeEntry
        "Plot"
        ""
        [ mkTypeEntry "Plot" "A plot for a chart." $ MkBoundType plotGroundType
        , mkTypeEntry "PointStyle" "Point style" $ MkBoundType pointStyleGroundType
        , mkValEntry "joinPlot" "Join plots" langJoinPlot
        , mkValEntry "plotPoints" "A plot from points" plotPoints
        , mkValEntry "plotHidden" "Hidden points that nevertheless affect axis scaling" plotHidden
        ]

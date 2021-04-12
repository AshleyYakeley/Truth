{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Drawing
    ( drawingLibraryModule
    , LangDrawing(..)
    ) where

import Data.Shim
import GI.Cairo.Render
import Language.Expression.Dolan
import Pinafore.Language.API
import Shapes

newtype LangDrawing = MkLangDrawing
    { unLangDrawing :: Render ()
    }

instance Semigroup LangDrawing where
    MkLangDrawing p <> MkLangDrawing q = MkLangDrawing $ p >> q

instance Monoid LangDrawing where
    mempty = MkLangDrawing $ return ()

drawingGroundType :: PinaforeGroundType '[] LangDrawing
drawingGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangDrawing)|]) "Drawing"

-- LangDrawing
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangDrawing where
    toShimWit = mkShimWit $ GroundDolanSingularType drawingGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangDrawing where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangDrawing where
    fromShimWit = mkShimWit $ GroundDolanSingularType drawingGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangDrawing where
    fromShimWit = singleDolanShimWit fromJMShimWit

drawingLibraryModule :: LibraryModule
drawingLibraryModule =
    MkDocTree
        "Drawing"
        ""
        [ mkTypeEntry "Drawing" "Something that can be drawn." $ MkBoundType drawingGroundType
        , mkValEntry "blank" "Blank drawing" $ mempty @LangDrawing
        , mkValEntry "concat" "Overlay drawings" $ mconcat @LangDrawing
        ]

module Pinafore.Query.Predefined
    ( predefinedBindings
    , predefinedDoc
    ) where

import Pinafore.AsText
import Pinafore.Edit
import Pinafore.Morphism
import Pinafore.Query.Expression
import Pinafore.Query.Value
import Shapes
import Truth.Core

valSpec ::
       AsText val
    => UISpec (WholeEdit (Maybe val))
    -> PinaforeLensValue (WholeEdit (Maybe Point))
    -> UISpec PinaforeEdit
valSpec spec val = uiLens (applyPinaforeLens literalPinaforeLensMorphism val) spec

pb :: forall t. ToQValue t
   => String
   -> t
   -> (QBindings, (String, String))
pb name val = (qbind name val, (name, qTypeDescriptionTo @t))

predefinitions :: [(QBindings, (String, String))]
predefinitions =
    [ pb "ui_textentry" $ valSpec $ uiNothingValue mempty uiTextEntry
    , pb "ui_textarea" $ valSpec $ uiNothingValue mempty $ uiConvert uiTextText
    , pb "ui_labelled" $ \text -> uiLabelled $ unpack (text :: Text)
    , pb "ui_vertical" uiVertical
    , pb "ui_pages" uiPages
        -- CSS
        -- drag
        -- icon
        --, pb "ui_checkbox" $ \name lens -> (uiLens lens $ uiCheckbox name :: UISpec PinaforeEdit)
    , pb "ui_pick" $ \(nameMorphism :: PinaforeFunctionMorphism Point (Maybe Text)) (fset :: PinaforeFunctionValue (FiniteSet Point)) -> let
          getName :: PinaforeFunctionMorphism Point (Maybe Point, String)
          getName =
              proc p -> do
                  n <- nameMorphism -< p
                  returnA -< (Just p, unpack $ fromMaybe mempty n)
          getNames :: PinaforeFunctionMorphism (FiniteSet Point) (FiniteSet (Maybe Point, String))
          getNames =
              proc fsp -> do
                  pairs <- cfmap getName -< fsp
                  returnA -< insertSet (Nothing, "") pairs
          opts :: EditFunction PinaforeEdit (ListEdit [(Maybe Point, String)] (WholeEdit (Maybe Point, String)))
          opts =
              (orderedKeyList @(FiniteSet (Maybe Point, String)) $ \(_, a) (_, b) -> compare a b) .
              convertEditFunction . applyPinaforeFunction getNames fset
          in uiOption @PinaforeEdit @(Maybe Point) opts
        -- switch
    , pb "ui_table" $ \cols (asp :: Point -> Result String (Text, UISpec PinaforeEdit)) (val :: PinaforeLensValue (FiniteSetEdit Point)) -> let
          showCell :: Maybe String -> (String, TableCellProps)
          showCell (Just s) = (s, tableCellPlain)
          showCell Nothing = ("empty", tableCellPlain {tcItalic = True})
          mapLens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeFunctionValue (String, TableCellProps)
          mapLens lens =
              funcEditFunction showCell . editLensFunction (applyPinaforeLens literalPinaforeLensMorphism lens)
          getColumn ::
                 (Text, Point -> Result String (PinaforeLensValue (WholeEdit (Maybe Point))))
              -> KeyColumn PinaforeEdit Point
          getColumn (name, f) =
              readOnlyKeyColumn (unpack name) $ \p ->
                  resultToM $ do
                      lens <- f p
                      return $ mapLens lens
          aspect :: Point -> IO (Maybe (String, UISpec PinaforeEdit))
          aspect point = resultToM $ fmap (return . first unpack) $ asp point
          in uiTable (fmap getColumn cols) aspect val
    ]

pd :: forall t. ToQValue t
   => String
   -> t
   -> (String, String)
pd name _ = (name, qTypeDescriptionTo @t)

predefinedDoc :: [(String, String)]
predefinedDoc = [pd "($)" qapply, pd "(.)" qcombine, pd "(&)" qmeet, pd "(|)" qjoin] ++ fmap snd predefinitions

predefinedBindings :: QBindings
predefinedBindings = mconcat $ fmap fst predefinitions

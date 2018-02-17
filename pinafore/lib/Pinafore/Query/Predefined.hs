module Pinafore.Query.Predefined
    ( predefinedBindings
    , predefinedDoc
    ) where

--import Pinafore.AsText
import Pinafore.Edit
import Pinafore.Morphism
import Pinafore.Query.Expression
import Pinafore.Query.Value
import Shapes
import Truth.Core

{-
valSpec ::
       AsText val
    => UISpec (WholeEdit (Maybe val))
    -> PinaforeLensValue (WholeEdit (Maybe Point))
    -> UISpec PinaforeEdit
valSpec spec val = uiLens (applyPinaforeLens literalPinaforeLensMorphism val) spec
-}
valSpecText :: UISpec (WholeEdit (Maybe Text)) -> PinaforeLensValue (WholeEdit (Maybe Text)) -> UISpec PinaforeEdit
valSpecText spec val = uiLens val spec

pb :: forall t. ToQValue t
   => Symbol
   -> t
   -> (QBindings, (Symbol, Text))
pb name val = (qbind name val, (name, qTypeDescriptionTo @t))

predefinitions :: [(QBindings, (Symbol, Text))]
predefinitions =
    [ pb "ui_textentry" $ valSpecText $ uiNothingValue mempty uiTextEntry
    , pb "ui_textarea" $ valSpecText $ uiNothingValue mempty $ uiConvert uiText
    , pb "ui_label" $ valSpecText $ uiNothingValue mempty $ uiLabel
    , pb "ui_horizontal" uiHorizontal
    , pb "ui_vertical" uiVertical
    , pb "ui_pages" uiPages
        -- CSS
        -- drag
        -- icon
        --, pb "ui_checkbox" $ \name lens -> (uiLens lens $ uiCheckbox name :: UISpec PinaforeEdit)
    , pb "ui_pick" $ \(nameMorphism :: PinaforeFunctionMorphism Point (Maybe Text)) (fset :: PinaforeFunctionValue (FiniteSet Point)) -> let
          getName :: PinaforeFunctionMorphism Point (Maybe Point, Text)
          getName =
              proc p -> do
                  n <- nameMorphism -< p
                  returnA -< (Just p, fromMaybe mempty n)
          getNames :: PinaforeFunctionMorphism (FiniteSet Point) (FiniteSet (Maybe Point, Text))
          getNames =
              proc fsp -> do
                  pairs <- cfmap getName -< fsp
                  returnA -< insertSet (Nothing, "") pairs
          opts :: EditFunction PinaforeEdit (ListEdit [(Maybe Point, Text)] (WholeEdit (Maybe Point, Text)))
          opts =
              (orderedKeyList @(FiniteSet (Maybe Point, Text)) $ \(_, a) (_, b) -> compare a b) .
              convertEditFunction . applyPinaforeFunction getNames fset
          in uiOption @PinaforeEdit @(Maybe Point) opts
        -- switch
    , pb "ui_table" $ \cols (asp :: Point -> Result Text (Text, UISpec PinaforeEdit)) (val :: PinaforeLensValue (FiniteSetEdit Point)) -> let
          showCell :: Maybe Text -> (Text, TableCellProps)
          showCell (Just s) = (s, tableCellPlain)
          showCell Nothing = ("empty", tableCellPlain {tcItalic = True})
          mapLens :: PinaforeLensValue (WholeEdit (Maybe Point)) -> PinaforeFunctionValue (Text, TableCellProps)
          mapLens lens =
              funcEditFunction showCell . editLensFunction (applyPinaforeLens literalPinaforeLensMorphism lens)
          getColumn ::
                 (Text, Point -> Result Text (PinaforeLensValue (WholeEdit (Maybe Point))))
              -> KeyColumn PinaforeEdit Point
          getColumn (name, f) =
              readOnlyKeyColumn name $ \p ->
                  resultToM $
                  mapResultFailure unpack $ do
                      lens <- f p
                      return $ mapLens lens
          aspect :: Point -> IO (Maybe (Text, UISpec PinaforeEdit))
          aspect point = resultToM $ mapResultFailure unpack $ fmap return $ asp point
          in uiTable (fmap getColumn cols) aspect val
    ]

pd :: forall t. ToQValue t
   => Symbol
   -> t
   -> (Symbol, Text)
pd name _ = (name, qTypeDescriptionTo @t)

predefinedDoc :: [(Symbol, Text)]
predefinedDoc = [pd "($)" qapply, pd "(.)" qcombine, pd "(&)" qmeet, pd "(|)" qjoin] ++ fmap snd predefinitions

predefinedBindings :: QBindings
predefinedBindings = mconcat $ fmap fst predefinitions

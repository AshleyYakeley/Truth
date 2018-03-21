module Pinafore.Query.Predefined
    ( predefinedBindings
    , predefinedDoc
    ) where

import Pinafore.Morphism
import Pinafore.Number
import Pinafore.Query.Expression
import Pinafore.Query.Value
import Pinafore.Table
import Shapes
import Truth.Core

valSpecText :: UISpec (WholeEdit (Maybe Text)) -> PinaforeLensValue baseedit (WholeEdit (Maybe Text)) -> UISpec baseedit
valSpecText spec val = uiLens val spec

pb :: forall baseedit t. ToQValue baseedit t
   => Symbol
   -> t
   -> (QBindings baseedit, (Symbol, Text))
pb name val = (qbind name val, (name, qTypeDescriptionTo @baseedit @t))

isUnit :: Bijection (Maybe ()) Bool
isUnit =
    MkBijection isJust $ \b ->
        if b
            then Just ()
            else Nothing

predefinitions ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => [(QBindings baseedit, (Symbol, Text))]
predefinitions =
    [ pb "$" $ qapply @baseedit
    , pb "." $ qcombine @baseedit
    , pb "&" $ qmeet @baseedit
    , pb "|" $ qjoin @baseedit
    , pb "++" $ qappend @baseedit
    , pb "==" $ liftA2 @(Literal baseedit) $ (==) @Text
    , pb "/=" $ liftA2 @(Literal baseedit) $ (/=) @Text
    , pb "+" $ liftA2 @(Literal baseedit) $ (+) @Number
    , pb "-" $ liftA2 @(Literal baseedit) $ (-) @Number
    , pb "*" $ liftA2 @(Literal baseedit) $ (*) @Number
    , pb "/" $ liftA2 @(Literal baseedit) $ (/) @Number
    , pb "~==" $ liftA2 @(Literal baseedit) $ (==) @Number
    , pb "~/=" $ liftA2 @(Literal baseedit) $ (/=) @Number
    , pb "<" $ liftA2 @(Literal baseedit) $ (<) @Number
    , pb "<=" $ liftA2 @(Literal baseedit) $ (<=) @Number
    , pb ">" $ liftA2 @(Literal baseedit) $ (>) @Number
    , pb ">=" $ liftA2 @(Literal baseedit) $ (>=) @Number
    , pb "ui_unitcheckbox" $ \name val -> uiLens (toEditLens isUnit . val) $ uiCheckbox name
    , pb "ui_booleancheckbox" $ \name val -> uiLens val $ uiMaybeCheckbox name
    , pb "ui_textentry" $ valSpecText $ uiNothingValue mempty uiTextEntry
    , pb "ui_textarea" $ valSpecText $ uiNothingValue mempty $ uiConvert uiText
    , pb "ui_label" $ valSpecText $ uiNothingValue mempty $ uiLabel
    , pb "ui_horizontal" uiHorizontal
    , pb "ui_vertical" uiVertical
    , pb "ui_pages" uiPages
        -- CSS
        -- drag
        -- icon
    , pb "ui_addbutton" $ \(name :: PinaforeFunctionValue baseedit (Maybe Text)) (val :: PinaforeLensValue baseedit (FiniteSetEdit Point)) ->
          uiTableNewItemButton (funcEditFunction (fromMaybe mempty) . name) val
    , pb "ui_windowbutton" $ \(name :: PinaforeFunctionValue baseedit (Maybe Text)) window ->
          uiButton (funcEditFunction (fromMaybe mempty) . name) $ viewOpenWindow window
    , pb "ui_pick" $ \(nameMorphism :: PinaforeFunctionMorphism baseedit Point (Maybe Text)) (fset :: PinaforeFunctionValue baseedit (FiniteSet Point)) -> let
          getName :: PinaforeFunctionMorphism baseedit Point (Maybe Point, Text)
          getName =
              proc p -> do
                  n <- nameMorphism -< p
                  returnA -< (Just p, fromMaybe mempty n)
          getNames :: PinaforeFunctionMorphism baseedit (FiniteSet Point) (FiniteSet (Maybe Point, Text))
          getNames =
              proc fsp -> do
                  pairs <- cfmap getName -< fsp
                  returnA -< insertSet (Nothing, "") pairs
          opts :: EditFunction baseedit (ListEdit [(Maybe Point, Text)] (WholeEdit (Maybe Point, Text)))
          opts =
              (orderedKeyList @(FiniteSet (Maybe Point, Text)) $ \(_, a) (_, b) -> compare a b) .
              convertEditFunction . applyPinaforeFunction getNames fset
          in uiOption @baseedit @(Maybe Point) opts
        -- switch
    , pb "ui_table" $ \cols (asp :: Point -> Result Text (UIWindow baseedit)) (val :: PinaforeLensValue baseedit (FiniteSetEdit Point)) -> let
          showCell :: Maybe Text -> (Text, TableCellProps)
          showCell (Just s) = (s, tableCellPlain)
          showCell Nothing = ("empty", tableCellPlain {tcItalic = True})
          mapLens ::
                 PinaforeLensValue baseedit (WholeEdit (Maybe Point))
              -> PinaforeFunctionValue baseedit (Text, TableCellProps)
          mapLens lens =
              funcEditFunction showCell . editLensFunction (applyPinaforeLens literalPinaforeLensMorphism lens)
          getColumn ::
                 ( PinaforeFunctionValue baseedit (Maybe Text)
                 , Point -> Result Text (PinaforeLensValue baseedit (WholeEdit (Maybe Point))))
              -> KeyColumn baseedit Point
          getColumn (name, f) =
              readOnlyKeyColumn (funcEditFunction (fromMaybe mempty) . name) $ \p ->
                  resultToM $
                  mapResultFailure unpack $ do
                      lens <- f p
                      return $ mapLens lens
          aspect :: Point -> Aspect baseedit
          aspect point = resultToM $ mapResultFailure unpack $ fmap return $ asp point
          in uiTable (fmap getColumn cols) aspect val
    ]

pd :: forall baseedit t. ToQValue baseedit t
   => Symbol
   -> t
   -> (Symbol, Text)
pd name _ = (name, qTypeDescriptionTo @baseedit @t)

predefinedDoc ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => [(Symbol, Text)]
predefinedDoc = [pd @baseedit "@" $ qinvert @baseedit] ++ fmap snd (predefinitions @baseedit)

predefinedBindings :: HasPinaforeTableEdit baseedit => QBindings baseedit
predefinedBindings = mconcat $ fmap fst predefinitions

module Pinafore.Query.Predefined
    ( predefinedBindings
    , predefinedDoc
    , qdisplay
    ) where

import Pinafore.File
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.Query.Convert
import Pinafore.Query.Expression
import Pinafore.Query.Literal
import Pinafore.Query.Value
import Pinafore.Table
import Shapes
import Truth.Core
import Truth.World.File
import Truth.World.ObjectStore

qcombine :: HasPinaforeTableEdit baseedit => QValue baseedit -> QValue baseedit -> QValue baseedit
qcombine (MkAny QTMorphism g) (MkAny QTMorphism f) = MkAny QTMorphism $ g . f
qcombine (MkAny QTInverseMorphism g) (MkAny QTInverseMorphism f) = MkAny QTInverseMorphism $ g . f
qcombine g f = MkAny QTFunction $ qapply g . qapply f

qmeet :: QSet baseedit -> QSet baseedit -> QSet baseedit
qmeet a b = readOnlyEditLens meetEditFunction . pairJoinEditLenses a b

qjoin :: QSet baseedit -> QSet baseedit -> QSet baseedit
qjoin a b = readOnlyEditLens joinEditFunction . pairJoinEditLenses a b

qdisplay :: HasPinaforeTableEdit baseedit => QValue baseedit -> PinaforeFunctionValue baseedit (FiniteSet Text)
qdisplay val =
    case fromQValue val of
        SuccessResult a -> a
        FailureResult _ -> constEditFunction $ opoint $ pack $ show val

qappend :: Literal baseedit Text -> Literal baseedit Text -> Literal baseedit Text
qappend = liftA2 (<>)

valSpecText :: UISpec (WholeEdit (Maybe Text)) -> QLiteral baseedit Text -> UISpec baseedit
valSpecText spec val = uiLens val spec

pb :: forall baseedit t. ToQValue baseedit t
   => Symbol
   -> t
   -> (QBindings baseedit, (Symbol, Text))
pb name val = (qbind name val, (name, qTypeDescription @t))

isUnit :: Bijection (Maybe ()) Bool
isUnit =
    MkBijection isJust $ \b ->
        if b
            then Just ()
            else Nothing

clearText :: EditFunction (WholeEdit (Maybe Text)) (WholeEdit Text)
clearText = funcEditFunction (fromMaybe mempty)

genpoint :: QActionM baseedit Point
genpoint = liftIO $ newKeyContainerItem @(FiniteSet Point)

newpoint :: forall baseedit. QSet baseedit -> (Point -> QAction baseedit) -> QAction baseedit
newpoint set continue = do
    point <- genpoint
    liftOuter $ mapViewEdit set $ viewObjectPushEdit $ \_ push -> push [KeyInsertReplaceItem point]
    continue point

getQImPoint :: QImPoint baseedit -> QActionM baseedit Point
getQImPoint qp =
    liftOuter $ do
        mpoint <- viewObjectRead $ \_ mr -> editFunctionRead qp mr ReadWhole
        case mpoint of
            Just point -> return point
            Nothing -> liftIO $ newKeyContainerItem @(FiniteSet Point)

addpoint :: forall baseedit. QSet baseedit -> QImPoint baseedit -> QAction baseedit
addpoint set qp = do
    point <- getQImPoint qp
    liftOuter $ mapViewEdit set $ viewObjectPushEdit $ \_ push -> push [KeyInsertReplaceItem point]

removepoint :: forall baseedit. QSet baseedit -> QImPoint baseedit -> QAction baseedit
removepoint set qp = do
    point <- getQImPoint qp
    liftOuter $ mapViewEdit set $ viewObjectPushEdit $ \_ push -> push [KeyDeleteItem point]

importfile ::
       forall baseedit. HasPinaforeFileEdit baseedit
    => QSet baseedit
    -> (Point -> QAction baseedit)
    -> QAction baseedit
importfile set continue = do
    chooseFile <- actionRequest witChooseFile
    mpath <- liftIO chooseFile
    case mpath of
        Nothing -> return ()
        Just path -> do
            let sourceobject = fileObject path
            newpoint set $ \point -> do
                mdestobject <-
                    liftOuter $
                    mapViewEdit (tupleEditLens (MkFunctionSelector point) . pinaforeFileLens) $ do
                        MkObject {..} <- viewObject
                        liftIO $
                            runUnliftIO objRun $ do
                                pushEdit $ objEdit [SingleObjectDeleteCreate]
                                objRead ReadSingleObjectStore
                destobject <-
                    case mdestobject of
                        Nothing -> liftInner $ FailureResult $ fromString $ "failed to create object " ++ show point
                        Just object -> return object
                liftIO $ copyObject sourceobject destobject
                continue point

predefinitions ::
       forall baseedit. (HasPinaforeTableEdit baseedit, HasPinaforeFileEdit baseedit)
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
    , pb "inexact" $ fmap @(Literal baseedit) numberToDouble
    , pb "approximate" $ liftA2 @(Literal baseedit) approximate
    , pb "exists" $ \(val :: QImLiteral baseedit Text) ->
          (funcEditFunction (Just . isJust) . val :: QImLiteral baseedit Bool)
    , pb "pass" (return () :: QAction baseedit)
    , pb ">>" $ ((>>) :: QAction baseedit -> QAction baseedit -> QAction baseedit)
    , pb "newpoint" $ newpoint @baseedit
    , pb "addpoint" $ addpoint @baseedit
    , pb "removepoint" $ removepoint @baseedit
    , pb "importfile" $ importfile @baseedit
    , pb "openwindow" viewOpenWindow
    , pb "openselection" viewOpenSelection
    , pb "ui_blank" uiNull
    , pb "ui_unitcheckbox" $ \name val -> uiCheckbox (clearText . name) $ toEditLens isUnit . val
    , pb "ui_booleancheckbox" $ \name val -> uiMaybeCheckbox (clearText . name) val
    , pb "ui_textentry" $ valSpecText $ uiNothingValue mempty uiTextEntry
    , pb "ui_textarea" $ valSpecText $ uiNothingValue mempty $ uiConvert uiText
    , pb "ui_label" $ valSpecText $ uiNothingValue mempty $ uiLabel
    , pb "ui_horizontal" uiHorizontal
    , pb "ui_vertical" uiVertical
    , pb "ui_pages" uiPages
        -- CSS
        -- drag
        -- icon
    , pb "ui_button" $ \(name :: QImLiteral baseedit Text) action -> uiButton (clearText . name) action
    , pb "ui_pick" $ \(nameMorphism :: QImLiteralMorphism baseedit Text) (fset :: QImSet baseedit) -> let
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
    , pb "ui_table" $ \cols (asp :: Point -> Result Text (UIWindow baseedit)) (val :: QSet baseedit) -> let
          showCell :: Maybe Text -> (Text, TableCellProps)
          showCell (Just s) = (s, tableCellPlain)
          showCell Nothing = ("empty", tableCellPlain {tcItalic = True})
          mapLens :: QPoint baseedit -> PinaforeFunctionValue baseedit (Text, TableCellProps)
          mapLens lens =
              funcEditFunction showCell . editLensFunction (applyPinaforeLens literalPinaforeLensMorphism lens)
          getColumn :: (QImLiteral baseedit Text, Point -> Result Text (QPoint baseedit)) -> KeyColumn baseedit Point
          getColumn (name, f) =
              readOnlyKeyColumn (clearText . name) $ \p ->
                  resultToM $
                  mapResultFailure unpack $ do
                      lens <- f p
                      return $ mapLens lens
          aspect :: Point -> Aspect baseedit
          aspect point = resultToM $ mapResultFailure unpack $ fmap return $ asp point
          in uiTable (fmap getColumn cols) aspect val
    ]

pd :: forall t. HasQTypeDescription t
   => Symbol
   -> t
   -> (Symbol, Text)
pd name _ = (name, qTypeDescription @t)

predefinedDoc ::
       forall baseedit. (HasPinaforeTableEdit baseedit, HasPinaforeFileEdit baseedit)
    => [(Symbol, Text)]
predefinedDoc = [pd "@" $ qinvert @baseedit] ++ fmap snd (predefinitions @baseedit)

predefinedBindings :: (HasPinaforeTableEdit baseedit, HasPinaforeFileEdit baseedit) => QBindings baseedit
predefinedBindings = mconcat $ fmap fst predefinitions

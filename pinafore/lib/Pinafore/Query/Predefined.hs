module Pinafore.Query.Predefined
    ( predefinedBindings
    , predefinedDoc
    ) where

import Pinafore.File
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.Query.Convert
import Pinafore.Query.Expression
import Pinafore.Query.Literal
import Pinafore.Query.Order
import Pinafore.Query.Types
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

output ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => QImLiteral baseedit Text
    -> QAction baseedit
output val = do
    mtext <- qGetFunctionValue val
    for_ mtext $ \text -> liftIO $ putStr $ unpack text

outputln ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => QImLiteral baseedit Text
    -> QAction baseedit
outputln val = do
    mtext <- qGetFunctionValue val
    for_ mtext $ \text -> liftIO $ putStrLn $ unpack text

withset ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => QOrder baseedit
    -> QImSet baseedit
    -> (Point -> QAction baseedit)
    -> QAction baseedit
withset order set cont = do
    points <- qGetFunctionValue $ qOrderSet order set
    for_ points cont

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
getQImPoint qp = do
    mpoint <- qGetFunctionValue qp
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

file_import ::
       forall baseedit. HasPinaforeFileEdit baseedit
    => QSet baseedit
    -> (Point -> QAction baseedit)
    -> QAction baseedit
file_import set continue = do
    chooseFile <- actionRequest witChooseFile
    mpath <- liftIO chooseFile
    case mpath of
        Nothing -> return ()
        Just path -> do
            let sourceobject = fileObject path
            newpoint set $ \point -> do
                mdestobject <-
                    liftOuter $
                    mapViewEdit (pinaforeFileItemLens point) $ do
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

file_size :: Object ByteStringEdit -> IO Int64
file_size MkObject {..} = runUnliftIO objRun $ objRead ReadByteStringLength

ui_table ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => [(QImLiteral baseedit Text, Point -> Result Text (QLiteral baseedit Text))]
    -> (Point -> Result Text (UIWindow baseedit))
    -> QSet baseedit
    -> UISpec baseedit
ui_table cols asp val = let
    showCell :: Maybe Text -> (Text, TableCellProps)
    showCell (Just s) = (s, tableCellPlain)
    showCell Nothing = ("empty", tableCellPlain {tcItalic = True})
    mapLens :: QLiteral baseedit Text -> PinaforeFunctionValue baseedit (Text, TableCellProps)
    mapLens lens = funcEditFunction showCell . editLensFunction lens
    getColumn :: (QImLiteral baseedit Text, Point -> Result Text (QLiteral baseedit Text)) -> KeyColumn baseedit Point
    getColumn (name, f) =
        readOnlyKeyColumn (clearText . name) $ \p ->
            resultToM $
            mapResultFailure unpack $ do
                lens <- f p
                return $ mapLens lens
    aspect :: Point -> Aspect baseedit
    aspect point = resultToM $ mapResultFailure unpack $ fmap return $ asp point
    in uiTable (fmap getColumn cols) aspect val

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
    , pb "alphabetical" $ alphabetical @baseedit
    , pb "numerical" $ numerical @baseedit
    , pb "chronological" $ chronological @baseedit
    , pb "orders" $ orders @baseedit
    , pb "orderon" $ orderon @baseedit
    , pb "rev" $ rev @baseedit
    , pb "inexact" $ fmap @(Literal baseedit) numberToDouble
    , pb "approximate" $ liftA2 @(Literal baseedit) approximate
    , pb "exists" $ \(val :: QImLiteral baseedit Text) ->
          (funcEditFunction (Just . isJust) . val :: QImLiteral baseedit Bool)
    , pb "pass" (return () :: QAction baseedit)
    , pb ">>" $ ((>>) :: QAction baseedit -> QAction baseedit -> QAction baseedit)
    , pb "withset" $ withset @baseedit
    , pb "output" $ output @baseedit
    , pb "outputln" $ outputln @baseedit
    , pb "newpoint" $ newpoint @baseedit
    , pb "addpoint" $ addpoint @baseedit
    , pb "removepoint" $ removepoint @baseedit
    , pb "file_import" $ file_import @baseedit
    , pb "file_size" $ fmap @(Literal baseedit) file_size
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
    , pb "ui_table" $ ui_table @baseedit
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

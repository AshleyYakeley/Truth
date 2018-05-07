module Pinafore.Query.Predefined
    ( predefinedBindings
    , predefinedDoc
    , outputln
    ) where

import Pinafore.File
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.PredicateMorphism
import Pinafore.Query.Convert
import Pinafore.Query.Expression
import Pinafore.Query.Lifted
import Pinafore.Query.Order
import Pinafore.Query.Types
import Pinafore.Query.Value
import Pinafore.Table
import Shapes
import Truth.Core
import Truth.World.File
import Truth.World.ObjectStore

qcombine :: HasPinaforeEntityEdit baseedit => QValue baseedit -> QValue baseedit -> QValue baseedit
qcombine (MkAny QTMorphism g) (MkAny QTMorphism f) = MkAny QTMorphism $ g . f
qcombine (MkAny QTInverseMorphism g) (MkAny QTInverseMorphism f) = MkAny QTInverseMorphism $ g . f
qcombine g f = MkAny QTFunction $ qapply g . qapply f

qmeet :: QRefSetPoint baseedit -> QRefSetPoint baseedit -> QRefSetPoint baseedit
qmeet a b = readOnlyEditLens meetEditFunction . pairJoinEditLenses a b

qjoin :: QRefSetPoint baseedit -> QRefSetPoint baseedit -> QRefSetPoint baseedit
qjoin a b = readOnlyEditLens joinEditFunction . pairJoinEditLenses a b

set_member :: Point -> FiniteSet Point -> Bool
set_member p set = elem p set

output ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QLiteral baseedit Text
    -> QAction baseedit
output val = do
    mtext <- qGetFunctionValue val
    for_ mtext $ \text -> liftIO $ putStr $ unpack text

outputln ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QLiteral baseedit Text
    -> QAction baseedit
outputln val = do
    mtext <- qGetFunctionValue val
    for_ mtext $ \text -> liftIO $ putStrLn $ unpack text

setcount :: FiniteSet Literal -> Int
setcount = olength

setsum :: FiniteSet Number -> Number
setsum (MkFiniteSet s) = sum s

setmean :: FiniteSet Number -> Number
setmean (MkFiniteSet s) = sum s / fromIntegral (olength s)

withset ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QOrder baseedit
    -> QSetPoint baseedit
    -> (Point -> QAction baseedit)
    -> QAction baseedit
withset order set cont = do
    points <- qGetFunctionValue $ qOrderSet order set
    for_ points cont

qappend :: Lifted baseedit Text -> Lifted baseedit Text -> Lifted baseedit Text
qappend = liftA2 (<>)

valSpecText :: UISpec (WholeEdit (Maybe Text)) -> QRefLiteral baseedit Text -> UISpec baseedit
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

genentity :: QActionM baseedit Point
genentity = liftIO $ newKeyContainerItem @(FiniteSet Point)

newentity :: forall baseedit. QRefSetPoint baseedit -> (Point -> QAction baseedit) -> QAction baseedit
newentity set continue = do
    entity <- genentity
    liftOuter $ mapViewEdit set $ viewObjectPushEdit $ \_ push -> push [KeyInsertReplaceItem entity]
    continue entity

getQImPoint :: QPoint baseedit -> QActionM baseedit Point
getQImPoint = qGetFunctionValue

addentity :: forall baseedit. QRefSetPoint baseedit -> QPoint baseedit -> QAction baseedit
addentity set qp = do
    entity <- getQImPoint qp
    liftOuter $ mapViewEdit set $ viewObjectPushEdit $ \_ push -> push [KeyInsertReplaceItem entity]

removeentity :: forall baseedit. QRefSetPoint baseedit -> QPoint baseedit -> QAction baseedit
removeentity set qp = do
    entity <- getQImPoint qp
    liftOuter $ mapViewEdit set $ viewObjectPushEdit $ \_ push -> push [KeyDeleteItem entity]

setentity :: forall baseedit. QRefPoint baseedit -> QPoint baseedit -> QAction baseedit
setentity var val = do
    p :: Point <- getQImPoint val
    liftOuter $ mapViewEdit var $ viewObjectPushEdit $ \_ push -> push [MkWholeEdit p]

file_import ::
       forall baseedit. HasPinaforeFileEdit baseedit
    => QRefSetPoint baseedit
    -> (Point -> QAction baseedit)
    -> QAction baseedit
file_import set continue = do
    chooseFile <- actionRequest witChooseFile
    mpath <- liftIO chooseFile
    case mpath of
        Nothing -> return ()
        Just path -> do
            let sourceobject = fileObject path
            newentity set $ \entity -> do
                mdestobject <-
                    liftOuter $
                    mapViewEdit (pinaforeFileItemLens entity) $ do
                        MkObject {..} <- viewObject
                        liftIO $
                            runUnliftIO objRun $ do
                                pushEdit $ objEdit [SingleObjectDeleteCreate]
                                objRead ReadSingleObjectStore
                destobject <-
                    case mdestobject of
                        Nothing -> liftInner $ FailureResult $ fromString $ "failed to create object " ++ show entity
                        Just object -> return object
                liftIO $ copyObject sourceobject destobject
                continue entity

file_size :: Object ByteStringEdit -> IO Int64
file_size MkObject {..} = runUnliftIO objRun $ objRead ReadByteStringLength

ui_table ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => [(QLiteral baseedit Text, Point -> Result Text (QRefLiteral baseedit Text))]
    -> (Point -> Result Text (UIWindow baseedit))
    -> QRefSetPoint baseedit
    -> UISpec baseedit
ui_table cols asp val = let
    showCell :: Maybe Text -> (Text, TableCellProps)
    showCell (Just s) = (s, tableCellPlain)
    showCell Nothing = ("empty", tableCellPlain {tcItalic = True})
    mapLens :: QRefLiteral baseedit Text -> PinaforeFunctionValue baseedit (Text, TableCellProps)
    mapLens lens = funcEditFunction showCell . editLensFunction lens
    getColumn :: (QLiteral baseedit Text, Point -> Result Text (QRefLiteral baseedit Text)) -> KeyColumn baseedit Point
    getColumn (name, f) =
        readOnlyKeyColumn (clearText . name) $ \p ->
            resultToM $
            mapResultFailure unpack $ do
                lens <- f p
                return $ mapLens lens
    aspect :: Point -> Aspect baseedit
    aspect entity = resultToM $ mapResultFailure unpack $ fmap return $ asp entity
    in uiTable (fmap getColumn cols) aspect val

literal_conv :: Literal -> Literal
literal_conv = id

ui_pick ::
       forall baseedit. QMorphismLiteral baseedit Text -> QSetPoint baseedit -> QRefPoint baseedit -> UISpec baseedit
ui_pick nameMorphism fset = let
    getName :: PinaforeFunctionMorphism baseedit Point (Point, Text)
    getName =
        proc p -> do
            n <- nameMorphism -< p
            returnA -< (p, fromMaybe mempty n)
    getNames :: PinaforeFunctionMorphism baseedit (FiniteSet Point) (FiniteSet (Point, Text))
    getNames =
        proc fsp -> do
            pairs <- cfmap getName -< fsp
            returnA -< pairs
            -- returnA -< insertSet (Nothing, "") pairs
    opts :: EditFunction baseedit (ListEdit [(Point, Text)] (WholeEdit (Point, Text)))
    opts =
        (orderedKeyList @(FiniteSet (Point, Text)) $ \(_, a) (_, b) -> compare a b) .
        convertEditFunction . applyPinaforeFunction getNames fset
    in uiOption @baseedit @Point opts

qfail :: forall baseedit. Lifted baseedit Text -> QAction baseedit
qfail lt = do
    mt <- unLifted lt
    liftIO $ fail $ unpack $ fromMaybe "<null>" mt

predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => [(QBindings baseedit, (Symbol, Text))]
predefinitions =
    [ pb "$" $ qapply @baseedit
    , pb "literal" $ fmap @(Lifted baseedit) $ literal_conv
    , pb "null" $ nullLifted @baseedit @Literal
    , pb "." $ qcombine @baseedit
    , pb "&" $ qmeet @baseedit
    , pb "|" $ qjoin @baseedit
    , pb "member" $ liftA2 @(Lifted baseedit) $ set_member
    , pb "++" $ qappend @baseedit
    , pb "==" $ liftA2 @(Lifted baseedit) $ (==) @Point
    , pb "/=" $ liftA2 @(Lifted baseedit) $ (/=) @Point
    , pb "+" $ liftA2 @(Lifted baseedit) $ (+) @Number
    , pb "-" $ liftA2 @(Lifted baseedit) $ (-) @Number
    , pb "*" $ liftA2 @(Lifted baseedit) $ (*) @Number
    , pb "/" $ liftA2 @(Lifted baseedit) $ (/) @Number
    , pb "~==" $ liftA2 @(Lifted baseedit) $ (==) @Number
    , pb "~/=" $ liftA2 @(Lifted baseedit) $ (/=) @Number
    , pb "<" $ liftA2 @(Lifted baseedit) $ (<) @Number
    , pb "<=" $ liftA2 @(Lifted baseedit) $ (<=) @Number
    , pb ">" $ liftA2 @(Lifted baseedit) $ (>) @Number
    , pb ">=" $ liftA2 @(Lifted baseedit) $ (>=) @Number
    , pb "abs" $ fmap @(Lifted baseedit) $ abs @Number
    , pb "signum" $ fmap @(Lifted baseedit) $ signum @Number
    , pb "count" $ fmap @(Lifted baseedit) setcount
    , pb "sum" $ fmap @(Lifted baseedit) setsum
    , pb "mean" $ fmap @(Lifted baseedit) setmean
    , pb "alphabetical" $ alphabetical @baseedit
    , pb "numerical" $ numerical @baseedit
    , pb "chronological" $ chronological @baseedit
    , pb "orders" $ orders @baseedit
    , pb "orderon" $ orderon @baseedit
    , pb "rev" $ rev @baseedit
    , pb "inexact" $ fmap @(Lifted baseedit) numberToDouble
    , pb "approximate" $ liftA2 @(Lifted baseedit) approximate
    , pb "exists" $ \(val :: QLiteral baseedit Literal) ->
          (funcEditFunction (Just . isJust) . val :: QLiteral baseedit Bool)
    , pb "pass" (return () :: QAction baseedit)
    , pb ">>" $ ((>>) :: QAction baseedit -> QAction baseedit -> QAction baseedit)
    , pb "fail" $ qfail @baseedit
    , pb "withset" $ withset @baseedit
    , pb "output" $ output @baseedit
    , pb "outputln" $ outputln @baseedit
    , pb "setentity" $ setentity @baseedit
    , pb "newentity" $ newentity @baseedit
    , pb "addentity" $ addentity @baseedit
    , pb "removeentity" $ removeentity @baseedit
    , pb "file_import" $ file_import @baseedit
    , pb "file_size" $ fmap @(Lifted baseedit) file_size
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
    , pb "ui_button" $ \(name :: QLiteral baseedit Text) action -> uiButton (clearText . name) action
    , pb "ui_pick" $ ui_pick
        -- switch
    , pb "ui_table" $ ui_table @baseedit
    ]

pd :: forall t. HasQTypeDescription t
   => Symbol
   -> t
   -> (Symbol, Text)
pd name _ = (name, qTypeDescription @t)

predefinedDoc ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => [(Symbol, Text)]
predefinedDoc = [pd "@" $ qinvert @baseedit] ++ fmap snd (predefinitions @baseedit)

predefinedBindings :: (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit) => QBindings baseedit
predefinedBindings = mconcat $ fmap fst predefinitions

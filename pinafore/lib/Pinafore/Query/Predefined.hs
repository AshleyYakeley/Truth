module Pinafore.Query.Predefined
    ( DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedDoc
    , outputln
    ) where

import Pinafore.File
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.PredicateMorphism
import Pinafore.Query.Convert
import Pinafore.Query.Doc
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
import Truth.Debug.Object

qcompose :: HasPinaforeEntityEdit baseedit => QValue baseedit -> QValue baseedit -> QValue baseedit
qcompose (MkAny QTMorphism g) (MkAny QTMorphism f) = MkAny QTMorphism $ g . f
qcompose (MkAny QTInverseMorphism g) (MkAny QTInverseMorphism f) = MkAny QTInverseMorphism $ f . g
qcompose g f = MkAny QTFunction $ qapply g . qapply f

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

valSpecText :: UISpec seledit (WholeEdit (Maybe Text)) -> QRefLiteral baseedit Text -> UISpec seledit baseedit
valSpecText spec val = uiLens val spec

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
    qLiftView $ viewMapEdit set $ viewObjectPushEdit $ \_ push -> push [KeyInsertReplaceItem entity]
    continue entity

getQImPoint :: QPoint baseedit -> QActionM baseedit Point
getQImPoint = qGetFunctionValue

addentity :: forall baseedit. QRefSetPoint baseedit -> QPoint baseedit -> QAction baseedit
addentity set qp = do
    entity <- getQImPoint qp
    qLiftView $ viewMapEdit set $ viewObjectPushEdit $ \_ push -> push [KeyInsertReplaceItem entity]

removeentity :: forall baseedit. QRefSetPoint baseedit -> QPoint baseedit -> QAction baseedit
removeentity set qp = do
    entity <- getQImPoint qp
    qLiftView $ viewMapEdit set $ viewObjectPushEdit $ \_ push -> push [KeyDeleteItem entity]

removeall :: forall baseedit. QRefSetPoint baseedit -> QAction baseedit
removeall set = do qLiftView $ viewMapEdit set $ viewObjectPushEdit $ \_ push -> push [KeyClear]

setentity :: forall baseedit. QRefPoint baseedit -> QPoint baseedit -> QAction baseedit
setentity var val = do
    p :: Point <- getQImPoint val
    traceBracket "setpoint: push" $ qLiftView $ viewMapEdit var $ viewObjectPushEdit $ \_ push -> push [MkWholeEdit p]

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
                    qLiftView $
                    viewMapEdit (pinaforeFileItemLens entity) $ do
                        MkObject {..} <- viewObject
                        liftIO $
                            runUnliftIO objRun $ do
                                pushEdit $ objEdit [SingleObjectDeleteCreate]
                                objRead ReadSingleObjectStore
                destobject <-
                    case mdestobject of
                        Nothing -> qLiftResult $ FailureResult $ fromString $ "failed to create object " ++ show entity
                        Just object -> return object
                liftIO $ copyObject sourceobject destobject
                continue entity

file_size :: Object ByteStringEdit -> IO Int64
file_size MkObject {..} = runUnliftIO objRun $ objRead ReadByteStringLength

withSelection :: (Point -> QAction baseedit) -> QAction baseedit
withSelection cont = do
    mselection <- qLiftView viewGetSelection
    case mselection of
        Nothing -> return ()
        Just MkObject {..} -> do
            point <- liftIO $ runUnliftIO objRun $ objRead ReadWhole
            cont point

ui_table ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => [(QLiteral baseedit Text, Point -> Result Text (QRefLiteral baseedit Text))]
    -> (Point -> Result Text (UIWindow baseedit))
    -> QRefSetPoint baseedit
    -> UISpec (ConstEdit Point) baseedit
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
    aspect :: Point -> IO (UIWindow baseedit)
    aspect entity = resultToM $ mapResultFailure unpack $ asp entity
    in uiTable (fmap getColumn cols) aspect val

ui_pick ::
       forall baseedit seledit.
       QMorphismLiteral baseedit Text
    -> QSetPoint baseedit
    -> QRefPoint baseedit
    -> UISpec seledit baseedit
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

qsingle :: FiniteSet Literal -> Maybe Literal
qsingle = getSingle

immutableset :: FiniteSet Point -> FiniteSet Point
immutableset = id

qstatictype :: forall baseedit. QValue baseedit -> Text
qstatictype (MkAny t _) = pack $ show t

qstaticshow :: forall baseedit. QValue baseedit -> Text
qstaticshow v = pack $ show v

nulljoin :: forall baseedit. Lifted baseedit Literal -> Lifted baseedit Literal -> Lifted baseedit Literal
nulljoin lx ly = let
    qq :: Maybe Literal -> Maybe Literal -> Maybe Literal
    qq Nothing y = y
    qq x _ = x
    in maybeLifted $ qq <$> liftedMaybe lx <*> liftedMaybe ly

type BindDoc baseedit = (Maybe (QBindings baseedit), DefDoc)

mkDefEntry ::
       forall baseedit t. ToQValue baseedit t
    => Symbol
    -> Text
    -> t
    -> DocTreeEntry (BindDoc baseedit)
mkDefEntry name desc val = EntryDocTreeEntry (Just (qbind name val), mkDefDoc name desc val)

predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree (BindDoc baseedit)
predefinitions =
    MkDocTree
        "Predefined"
        [ docTreeEntry
              "Identity"
              [ mkDefEntry "is" "Entity identity. This is always `true` or `false`." $
                liftA2 @(Lifted baseedit) $ (==) @Point
              ]
        , docTreeEntry
              "Functions & Morphisms"
              [ mkDefEntry "$" "Apply a function, morphism, or inverse morphism to a value." $ qapply @baseedit
              , mkDefEntry "." "Compose functions, morphisms, or inverse morphisms." $ qcompose @baseedit
              , mkDefEntry "identity" "The identity morphism." $ (id :: QMorphismRefPoint baseedit)
              , EntryDocTreeEntry
                    ( Nothing
                    , mkDefDoc "@" "Invert a morphism to an inverse morphism, or an inverse morphism to a morphism." $
                      qinvert @baseedit)
              ]
        , docTreeEntry
              "Sets"
              [ mkDefEntry "/\\" "Intersection of sets. The resulting set can be added to, but not deleted from." $
                qmeet @baseedit
              , mkDefEntry "\\/" "Union of sets. The resulting set can be deleted from, but not added to." $
                qjoin @baseedit
              , mkDefEntry "member" "Determine membership of a set" $ liftA2 @(Lifted baseedit) $ set_member
              , mkDefEntry "single" "The member of a single-member set, or null." $ fmap @(Lifted baseedit) $ qsingle
              , mkDefEntry "immutableset" "A set as immutable." $ fmap @(Lifted baseedit) $ immutableset
              ]
        , docTreeEntry
              "Literals"
              [ mkDefEntry "==" "Literal equality. Note that `null == x` and `x == null` are null for any x." $
                liftA2 @(Lifted baseedit) $ (==) @Literal
              , mkDefEntry "/=" "Literal non-equality. Note that `null /= x` and `x /= null` are null for any x." $
                liftA2 @(Lifted baseedit) $ (/=) @Literal
              , docTreeEntry
                    "Nulls"
                    [ mkDefEntry
                          "null"
                          "Null inhabits every literal type, representing missing information. Note that `is null null` = `false`." $
                      nullLifted @baseedit @Literal
                    , mkDefEntry "exists" "True if the literal is not null." $ \(val :: QLiteral baseedit Literal) ->
                          (funcEditFunction (Just . isJust) . val :: QLiteral baseedit Bool)
                    , mkDefEntry "??" "`p ?? q` = `if exists p then p else q`." $ nulljoin @baseedit
                    ]
              , docTreeEntry
                    "Boolean"
                    [ mkDefEntry "&&" "Boolean AND." $ liftA2 @(Lifted baseedit) (&&)
                    , mkDefEntry "||" "Boolean OR." $ liftA2 @(Lifted baseedit) (||)
                    , mkDefEntry "not" "Boolean NOT." $ fmap @(Lifted baseedit) not
                    ]
              , docTreeEntry "Text" [mkDefEntry "++" "Concatenate text." $ qappend @baseedit]
              , docTreeEntry
                    "Numeric"
                    [ mkDefEntry "+" "Numeric add." $ liftA2 @(Lifted baseedit) $ (+) @Number
                    , mkDefEntry "-" "Numeric Subtract." $ liftA2 @(Lifted baseedit) $ (-) @Number
                    , mkDefEntry "*" "Numeric Multiply." $ liftA2 @(Lifted baseedit) $ (*) @Number
                    , mkDefEntry "/" "Numeric Divide." $ liftA2 @(Lifted baseedit) $ (/) @Number
                    , mkDefEntry "~==" "Numeric equality, folding exact and inexact numbers." $
                      liftA2 @(Lifted baseedit) $ (==) @Number
                    , mkDefEntry "~/=" "Numeric non-equality." $ liftA2 @(Lifted baseedit) $ (/=) @Number
                    , mkDefEntry "<" "Numeric strictly less." $ liftA2 @(Lifted baseedit) $ (<) @Number
                    , mkDefEntry "<=" "Numeric less or equal." $ liftA2 @(Lifted baseedit) $ (<=) @Number
                    , mkDefEntry ">" "Numeric strictly greater." $ liftA2 @(Lifted baseedit) $ (>) @Number
                    , mkDefEntry ">=" "Numeric greater or equal." $ liftA2 @(Lifted baseedit) $ (>=) @Number
                    , mkDefEntry "abs" "Numeric absolute value." $ fmap @(Lifted baseedit) $ abs @Number
                    , mkDefEntry "signum" "Numeric sign." $ fmap @(Lifted baseedit) $ signum @Number
                    , mkDefEntry "inexact" "Convert a number to inexact." $ fmap @(Lifted baseedit) numberToDouble
                    , mkDefEntry
                          "approximate"
                          "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`." $
                      liftA2 @(Lifted baseedit) approximate
                    ]
              ]
        , docTreeEntry
              "Set Aggregation"
              [ mkDefEntry "count" "Count of non-null literals in a set." $ fmap @(Lifted baseedit) setcount
              , mkDefEntry "sum" "Sum of numbers in a set." $ fmap @(Lifted baseedit) setsum
              , mkDefEntry "mean" "Mean of numbers in a set." $ fmap @(Lifted baseedit) setmean
              ]
        , docTreeEntry
              "Orders"
              [ mkDefEntry "alphabetical" "Alphabetical order." $ alphabetical @baseedit
              , mkDefEntry "numerical" "Numercal order." $ numerical @baseedit
              , mkDefEntry "chronological" "Chronological order." $ chronological @baseedit
              , mkDefEntry "orders" "Join orders by priority." $ orders @baseedit
              , mkDefEntry "orderon" "Order by an order on a particular morphism." $ orderon @baseedit
              , mkDefEntry "rev" "Reverse an order." $ rev @baseedit
              ]
        , docTreeEntry
              "Actions"
              [ mkDefEntry "pass" "Do nothing." (return () :: QAction baseedit)
              , mkDefEntry ">>" "Do actions in sequence." $
                ((>>) :: QAction baseedit -> QAction baseedit -> QAction baseedit)
              , mkDefEntry "fail" "Fail, causing the program to terminate with error." $ qfail @baseedit
              , mkDefEntry "withset" "Perform an action on every member of a set, in the given order." $
                withset @baseedit
              , mkDefEntry "output" "Output text to standard output." $ output @baseedit
              , mkDefEntry "outputln" "Output text and a newline to standard output." $ outputln @baseedit
              , mkDefEntry "setentity" "Set an entity reference to an entity." $ setentity @baseedit
              , mkDefEntry "newentity" "Create a new entity in a set and act on it." $ newentity @baseedit
              , mkDefEntry "addentity" "Add an entity to a set." $ addentity @baseedit
              , mkDefEntry "removeentity" "Remove an entity from a set." $ removeentity @baseedit
              , mkDefEntry "removeall" "Remove all entities from a set." $ removeall @baseedit
              ]
        , docTreeEntry
              "Files"
              [ mkDefEntry "file_import" "Import a file into a set." $ file_import @baseedit
              , mkDefEntry "file_size" "The size of a file." $ fmap @(Lifted baseedit) file_size
              ]
        , docTreeEntry
              "UI"
              [ mkDefEntry "openwindow" "Open a new window with this title and UI." viewOpenWindow
              , mkDefEntry "openselection" "Open the item selected in the UI of this window." viewOpenSelection
              , mkDefEntry "withselection" "Act with the item selected in the UI of this window." $
                withSelection @baseedit
              , mkDefEntry "ui_blank" "Blank user-interface" uiNull
              , mkDefEntry "ui_unitcheckbox" "(TBD)" $ \name val ->
                    uiCheckbox (clearText . name) $ toEditLens isUnit . val
              , mkDefEntry "ui_booleancheckbox" "Checkbox. Use shift-click to set to null." $ \name val ->
                    uiMaybeCheckbox (clearText . name) val
              , mkDefEntry "ui_textentry" "Text entry, empty text is null." $
                valSpecText $ uiNothingValue mempty uiTextEntry
              , mkDefEntry "ui_textarea" "Text area, empty text is null." $
                valSpecText $ uiNothingValue mempty $ uiNoSelectionLens $ uiConvert uiText
              , mkDefEntry "ui_label" "Label." $ valSpecText $ uiNothingValue mempty $ uiLabel
              , mkDefEntry
                    "ui_horizontal"
                    "Items arranged horizontally, each flag is whether to expand into remaining space."
                    uiHorizontal
              , mkDefEntry
                    "ui_vertical"
                    "Items arranged vertically, each flag is whether to expand into remaining space."
                    uiVertical
              , mkDefEntry
                    "ui_pages"
                    "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content."
                    uiPages
                -- CSS
                -- drag
                -- icon
              , mkDefEntry "ui_button" "A button with this text that does this action." $ \(name :: QLiteral baseedit Text) action ->
                    uiButton (clearText . name) action
              , mkDefEntry "ui_pick" "A drop-down menu." $ ui_pick
                -- switch
              , mkDefEntry
                    "ui_table"
                    "A list table. First arg is columns (name, property), second is the window to open for a selection, third is the set of items." $
                ui_table @baseedit
              ]
        , docTreeEntry
              "Static"
              [ mkDefEntry "static-type" "Get the static type of a value." $ qstatictype @baseedit
              , mkDefEntry "static-show" "Show a value statically." $ qstaticshow @baseedit
              ]
        ]

predefinedDoc ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree DefDoc
predefinedDoc = fmap snd $ predefinitions @baseedit

predefinedBindings ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => QBindings baseedit
predefinedBindings = mconcat $ catMaybes $ toList $ fmap fst $ predefinitions @baseedit

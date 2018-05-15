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

qcompose :: HasPinaforeEntityEdit baseedit => QValue baseedit -> QValue baseedit -> QValue baseedit
qcompose (MkAny QTMorphism g) (MkAny QTMorphism f) = MkAny QTMorphism $ g . f
qcompose (MkAny QTInverseMorphism g) (MkAny QTInverseMorphism f) = MkAny QTInverseMorphism $ g . f
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

pb :: forall baseedit t. ToQValue baseedit t
   => Symbol
   -> Text
   -> t
   -> (QBindings baseedit, (Symbol, Text, Text))
pb name desc val = (qbind name val, (name, qTypeDescription @t, desc))

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

setentity :: forall baseedit. QRefPoint baseedit -> QPoint baseedit -> QAction baseedit
setentity var val = do
    p :: Point <- getQImPoint val
    qLiftView $ viewMapEdit var $ viewObjectPushEdit $ \_ push -> push [MkWholeEdit p]

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

qshow :: forall baseedit. QValue baseedit -> Text
qshow v = pack $ show v

nulljoin :: forall baseedit. Lifted baseedit Literal -> Lifted baseedit Literal -> Lifted baseedit Literal
nulljoin lx ly = let
    qq :: Maybe Literal -> Maybe Literal -> Maybe Literal
    qq Nothing y = y
    qq x _ = x
    in maybeLifted $ qq <$> liftedMaybe lx <*> liftedMaybe ly

predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => [(QBindings baseedit, (Symbol, Text, Text))]
predefinitions =
    [ pb "show" "Show a value statically." $ qshow @baseedit
    -- Basic
    , pb "$" "Apply a function, morphism, or inverse morphism to a value." $ qapply @baseedit
    , pb "." "Compose functions, morphisms, or inverse morphisms." $ qcompose @baseedit
    -- Sets
    , pb "&" "Intersection of sets. The resulting set can be added to, but not deleted from." $ qmeet @baseedit
    , pb "|" "Union of sets. The resulting set can be deleted from, but not added to." $ qjoin @baseedit
    , pb "member" "Determine membership of a set" $ liftA2 @(Lifted baseedit) $ set_member
    , pb "single" "The member of a single-member set, or null." $ fmap @(Lifted baseedit) $ qsingle
    -- Literals
    , pb "==" "Equality. (TBD)" $ liftA2 @(Lifted baseedit) $ (==) @Point
    , pb "/=" "Non-equality. (TBD)" $ liftA2 @(Lifted baseedit) $ (/=) @Point
    -- Nulls
    , pb "null" "The null literal." $ nullLifted @baseedit @Literal
    , pb "exists" "True if the literal is not null." $ \(val :: QLiteral baseedit Literal) ->
          (funcEditFunction (Just . isJust) . val :: QLiteral baseedit Bool)
    , pb "??" "`p ?? q` = `if exists p then p else q`." $ nulljoin @baseedit
    -- Text
    , pb "++" "Concatenate text." $ qappend @baseedit
    -- Numeric
    , pb "+" "Numeric add." $ liftA2 @(Lifted baseedit) $ (+) @Number
    , pb "-" "Numeric Subtract." $ liftA2 @(Lifted baseedit) $ (-) @Number
    , pb "*" "Numeric Multiply." $ liftA2 @(Lifted baseedit) $ (*) @Number
    , pb "/" "Numeric Divide." $ liftA2 @(Lifted baseedit) $ (/) @Number
    , pb "~==" "Numeric equality, folding exact and inexact numbers." $ liftA2 @(Lifted baseedit) $ (==) @Number
    , pb "~/=" "Numeric non-equality." $ liftA2 @(Lifted baseedit) $ (/=) @Number
    , pb "<" "Numeric strictly less." $ liftA2 @(Lifted baseedit) $ (<) @Number
    , pb "<=" "Numeric less or equal." $ liftA2 @(Lifted baseedit) $ (<=) @Number
    , pb ">" "Numeric strictly greater." $ liftA2 @(Lifted baseedit) $ (>) @Number
    , pb ">=" "Numeric greater or equal." $ liftA2 @(Lifted baseedit) $ (>=) @Number
    , pb "abs" "Numeric absolute value." $ fmap @(Lifted baseedit) $ abs @Number
    , pb "signum" "Numeric sign." $ fmap @(Lifted baseedit) $ signum @Number
    , pb "inexact" "Convert a number to inexact." $ fmap @(Lifted baseedit) numberToDouble
    , pb "approximate" "`approximate d x` gives the exact number that's a multiple of `d` that's closest to `x`." $
      liftA2 @(Lifted baseedit) approximate
    -- Set Aggregation
    , pb "count" "Count of non-null literals in a set." $ fmap @(Lifted baseedit) setcount
    , pb "sum" "Sum of numbers in a set." $ fmap @(Lifted baseedit) setsum
    , pb "mean" "Mean of numbers in a set." $ fmap @(Lifted baseedit) setmean
    -- Orders
    , pb "alphabetical" "Alphabetical order." $ alphabetical @baseedit
    , pb "numerical" "Numercal order." $ numerical @baseedit
    , pb "chronological" "Chronological order." $ chronological @baseedit
    , pb "orders" "Join orders by priority." $ orders @baseedit
    , pb "orderon" "Order by an order on a particular morphism." $ orderon @baseedit
    , pb "rev" "Reverse an order." $ rev @baseedit
    -- Actions
    , pb "pass" "Do nothing." (return () :: QAction baseedit)
    , pb ">>" "Do actions in sequence." $ ((>>) :: QAction baseedit -> QAction baseedit -> QAction baseedit)
    , pb "fail" "Fail, causing the program to terminate with error." $ qfail @baseedit
    , pb "withset" "Perform an action on every member of a set, in the given order." $ withset @baseedit
    , pb "output" "Output text to standard output." $ output @baseedit
    , pb "outputln" "Output text and a newline to standard output." $ outputln @baseedit
    , pb "setentity" "Set an entity reference to an entity." $ setentity @baseedit
    , pb "newentity" "Create a new entity in a set and act on it." $ newentity @baseedit
    , pb "addentity" "Add an entity to a set." $ addentity @baseedit
    , pb "removeentity" "Remove an entity from a set." $ removeentity @baseedit
    -- Files
    , pb "file_import" "Import a file into a set." $ file_import @baseedit
    , pb "file_size" "The size of a file." $ fmap @(Lifted baseedit) file_size
    -- UI
    , pb "openwindow" "Open a new window with this title and UI." viewOpenWindow
    , pb "openselection" "Open the item selected in the UI of this window." viewOpenSelection
    , pb "withselection" "Act with the item selected in the UI of this window." $ withSelection @baseedit
    , pb "ui_blank" "Blank user-interface" uiNull
    , pb "ui_unitcheckbox" "(TBD)" $ \name val -> uiCheckbox (clearText . name) $ toEditLens isUnit . val
    , pb "ui_booleancheckbox" "Checkbox. Use ctrl-click to set to null." $ \name val ->
          uiMaybeCheckbox (clearText . name) val
    , pb "ui_textentry" "Text entry, empty text is null." $ valSpecText $ uiNothingValue mempty uiTextEntry
    , pb "ui_textarea" "Text area, empty text is null." $
      valSpecText $ uiNothingValue mempty $ uiNoSelectionLens $ uiConvert uiText
    , pb "ui_label" "Label." $ valSpecText $ uiNothingValue mempty $ uiLabel
    , pb "ui_horizontal"
          "Items arranged horizontally, each flag is whether to expand into remaining space."
          uiHorizontal
    , pb "ui_vertical" "Items arranged vertically, each flag is whether to expand into remaining space." uiVertical
    , pb "ui_pages"
          "A notebook of pages. First of each pair is for the page tab (typically a label), second is the content."
          uiPages
        -- CSS
        -- drag
        -- icon
    , pb "ui_button" "A button with this text that does this action." $ \(name :: QLiteral baseedit Text) action ->
          uiButton (clearText . name) action
    , pb "ui_pick" "A drop-down menu." $ ui_pick
        -- switch
    , pb "ui_table"
          "A list table. First arg is columns (name, property), second is the window to open for a selection, third is the set of items." $
      ui_table @baseedit
    ]

pd :: forall t. HasQTypeDescription t
   => Symbol
   -> Text
   -> t
   -> (Symbol, Text, Text)
pd name desc _ = (name, qTypeDescription @t, desc)

predefinedDoc ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => [(Symbol, Text, Text)]
predefinedDoc =
    [pd "@" "Invert a morphism to an inverse morphism, or an inverse morphism to a morphism." $ qinvert @baseedit] ++
    fmap snd (predefinitions @baseedit)

predefinedBindings :: (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit) => QBindings baseedit
predefinedBindings = mconcat $ fmap fst predefinitions

{-# OPTIONS -fno-warn-orphans #-}

module Changes.World.SQLite
    ( module Changes.World.SQLite
    , SQLData
    , FromField (..)
    , ToField (..)
    )
where

import Changes.Core
import Database.SQLite.Simple hiding (columnName)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Shapes
import System.Directory

import Changes.World.SQLite.Schema qualified as SQLite

data QueryString
    = MkQueryString
        Query
        [SQLData]

instance Semigroup QueryString where
    (MkQueryString qa va) <> (MkQueryString qb vb) = MkQueryString (mappend qa qb) (va <> vb)

instance Monoid QueryString where
    mempty = MkQueryString mempty mempty
    mappend = (<>)

instance IsString QueryString where
    fromString s = MkQueryString (fromString s) []

groupQueryStringsMap :: [QueryString] -> Map Query (NonEmpty [SQLData])
groupQueryStringsMap [] = mempty
groupQueryStringsMap (MkQueryString q d : qss) = let
    m = groupQueryStringsMap qss
    dd =
        case lookup q m of
            Nothing -> []
            Just ndd -> toList ndd
    in insertMap q (d :| dd) m

valQueryString :: ToField t => t -> QueryString
valQueryString t = MkQueryString (fromString "?") [toField t]

fromSQLData :: FromField a => SQLData -> Maybe a
fromSQLData d =
    case fromField $ Field d 0 of
        Ok a -> Just a
        Errors _ -> Nothing

convertField :: (ToField p, FromField q) => p -> Maybe q
convertField = fromSQLData . toField

maybeToOk :: Maybe a -> Ok a
maybeToOk (Just a) = Ok a
maybeToOk Nothing = Errors []

data SQLiteDatabase

type SQLiteReader tablesel = TupleDatabaseReader SQLiteDatabase tablesel

type SQLiteEdit tablesel = TupleDatabaseEdit SQLiteDatabase tablesel

type SQLiteUpdate tablesel = EditUpdate (SQLiteEdit tablesel)

type family RowColSel (row :: Type) :: Type -> Type

type instance RowColSel (AllOf colsel) = colsel

class HasSchema (t :: Type) where
    type Schema t :: Type
    schemaString :: Schema t -> t -> QueryString

data Expr colsel t where
    ConstExpr :: ToField t => t -> Expr colsel t
    ColumnExpr :: colsel t -> Expr colsel t
    EqualsExpr :: Eq t => Expr colsel t -> Expr colsel t -> Expr colsel Bool
    AndExpr :: Expr colsel Bool -> Expr colsel Bool -> Expr colsel Bool
    OrExpr :: Expr colsel Bool -> Expr colsel Bool -> Expr colsel Bool

instance AllConstraint Show colsel => Show (Expr colsel t) where
    show (ConstExpr t) = show $ toField t
    show (ColumnExpr col) = allShow col
    show (EqualsExpr ea eb) = "(" ++ show ea ++ "=" ++ show eb ++ ")"
    show (AndExpr ea eb) = "(" ++ show ea ++ " & " ++ show eb ++ ")"
    show (OrExpr ea eb) = "(" ++ show ea ++ " & " ++ show eb ++ ")"

instance Lattice (Expr colsel Bool) where
    (/\) = AndExpr
    (\/) = OrExpr

instance BoundedMeetSemiLattice (Expr colsel Bool) where
    top = ConstExpr True

instance BoundedJoinSemiLattice (Expr colsel Bool) where
    bottom = ConstExpr False

class ExprEquals expr where
    (===) :: Eq t => expr t -> expr t -> expr Bool

instance ExprEquals (Expr colsel) where
    (===) = EqualsExpr

evalExpr :: Applicative m => Expr colsel t -> (forall a. colsel a -> m a) -> m t
evalExpr (ConstExpr v) _ = pure v
evalExpr (ColumnExpr sel) tuple = tuple sel
evalExpr (EqualsExpr e1 e2) tuple = (==) <$> evalExpr e1 tuple <*> evalExpr e2 tuple
evalExpr (AndExpr e1 e2) tuple = (&&) <$> evalExpr e1 tuple <*> evalExpr e2 tuple
evalExpr (OrExpr e1 e2) tuple = (||) <$> evalExpr e1 tuple <*> evalExpr e2 tuple

data ColumnRefSchema t = MkColumnRefSchema
    { columnRefName :: String
    , columnRefType :: SQLite.ColumnTypeSchema t
    }

instance HasSchema (Expr colsel t) where
    type Schema (Expr colsel t) = FiniteAllFor ColumnRefSchema colsel
    schemaString _ (ConstExpr t) = valQueryString t
    schemaString csch (ColumnExpr col) = fromString $ columnRefName $ finiteGetAllFor csch col
    schemaString csch (EqualsExpr e1 e2) = "(" <> schemaString csch e1 <> "=" <> schemaString csch e2 <> ")"
    schemaString csch (AndExpr e1 e2) = "(" <> schemaString csch e1 <> " AND " <> schemaString csch e2 <> ")"
    schemaString csch (OrExpr e1 e2) = "(" <> schemaString csch e1 <> " OR " <> schemaString csch e2 <> ")"

class
    (FiniteWitness colsel, WitnessConstraint FromField colsel, WitnessConstraint ToField colsel) =>
    IsSQLiteTable colsel

instance
    (FiniteWitness colsel, WitnessConstraint FromField colsel, WitnessConstraint ToField colsel) =>
    IsSQLiteTable colsel

instance TupleDatabaseType SQLiteDatabase where
    type TupleDatabaseTypeRowWitness SQLiteDatabase = IsSQLiteTable
    type TupleExpr SQLiteDatabase colsel = Expr colsel
    evalTupleExpr expr (MkAllFor tuple) = evalExpr expr tuple
    constBoolExpr = ConstExpr
    columnExpr = ColumnExpr

instance AllConstraint Show colsel => AllConstraint Show (Expr colsel) where
    allConstraint = Dict

instance ShowableTupleDatabaseType SQLiteDatabase where
    witnessShowTupleExpr = Dict

instance (FiniteWitness colsel, WitnessConstraint FromField colsel) => FromRow (AllOf colsel) where
    fromRow =
        assembleAllOf $ \wt ->
            case witnessConstraint @Type @FromField wt of
                Dict -> fmap fromOnly fromRow

instance HasSchema (TupleWhereClause SQLiteDatabase row) where
    type Schema (TupleWhereClause SQLiteDatabase row) = FiniteAllFor ColumnRefSchema (RowColSel row)
    schemaString csch (MkTupleWhereClause expr) = schemaString csch expr

instance HasSchema (TupleSelectClause SQLiteDatabase tablesel row row') where
    type Schema (TupleSelectClause SQLiteDatabase tablesel row row') = FiniteAllFor ColumnRefSchema (RowColSel row)
    schemaString csch (MkTupleSelectClause mapSel) =
        intercalate "," $ fmap (\(MkSome cs') -> schemaString csch $ mapSel cs') $ allWitnesses @(RowColSel row')

instance HasSchema (TupleOrderItem colsel) where
    type Schema (TupleOrderItem colsel) = FiniteAllFor ColumnRefSchema colsel
    schemaString csch (MkTupleOrderItem col dir) =
        fromString $ (columnRefName $ finiteGetAllFor csch col) ++ " " ++ show dir

columnRef :: String -> SQLite.ColumnSchema t -> ColumnRefSchema t
columnRef tableRefName SQLite.MkColumnSchema{..} = let
    columnRefName =
        case tableRefName of
            "" -> columnName
            _ -> tableRefName ++ "." ++ columnName
    columnRefType = columnType
    in MkColumnRefSchema{..}

joinTableSchema ::
    forall tablesel row.
    FiniteAllFor SQLite.TableSchema tablesel ->
    TableJoin SQLiteDatabase (TupleTableSel tablesel) row ->
    State Int ([QueryString], FiniteAllFor ColumnRefSchema (RowColSel row))
joinTableSchema schema (SingleTable (MkTupleTableSel tsel)) = do
    i <- get
    put $ succ i
    let
        tableRefName = "t" ++ show i
        SQLite.MkTableSchema{..} = finiteGetAllFor schema tsel
        tabRefText = fromString $ tableName ++ " AS " ++ tableRefName
        colRefSchema = mapFiniteAllFor (columnRef tableRefName) tableColumns
    return ([tabRefText], colRefSchema)
joinTableSchema schema (JoinTables OuterTupleJoinClause j1 j2) = do
    (t1, s1) <- joinTableSchema schema j1
    (t2, s2) <- joinTableSchema schema j2
    return $ (t1 ++ t2, eitherFiniteAllFor s1 s2)

sqliteFilePathWitness :: IOWitness (ReaderT Connection)
sqliteFilePathWitness = $(iowitness [t|ReaderT Connection|])

sqliteReference ::
    forall tablesel.
    WitnessConstraint IsSQLiteTable tablesel =>
    FilePath ->
    SQLite.DatabaseSchema tablesel ->
    IO (Reference (SQLiteEdit tablesel))
sqliteReference path schema@SQLite.MkDatabaseSchema{..} = do
    var <- newMVar ()
    let
        objRun :: ResourceRunner '[ReaderT Connection]
        objRun =
            mkResourceRunner (hashOpenWitness sqliteFilePathWitness path) $ \call ->
                mVarRunLocked var $ do
                    exists <- liftIO $ doesFileExist path
                    liftIOWithUnlift $ \unlift ->
                        withConnection path $ \conn -> do
                            if exists
                                then return ()
                                else for_ (SQLite.toSchema schema) $ execute_ conn -- create the database if we're creating the file
                            runReaderT (hoist unlift call) conn
        wherePart :: Schema (TupleWhereClause SQLiteDatabase row) -> TupleWhereClause SQLiteDatabase row -> QueryString
        wherePart rowSchema wc =
            case wc of
                MkTupleWhereClause (ConstExpr True) -> ""
                _ -> " WHERE " <> schemaString rowSchema wc
        sqliteReadQuery :: SQLiteReader tablesel [AllOf colsel] -> QueryString
        sqliteReadQuery (DatabaseSelect jc wc oc sc) =
            case evalState (joinTableSchema databaseTables jc) 1 of
                (tabRefs, rowSchema) -> let
                    fromPart :: QueryString
                    fromPart =
                        case tabRefs of
                            [] -> ""
                            _ -> " FROM " <> intercalate "," tabRefs
                    orderPart :: QueryString
                    orderPart =
                        case oc of
                            MkTupleOrderClause [] -> ""
                            MkTupleOrderClause ocs ->
                                " ORDER BY " <> (intercalate "," $ fmap (schemaString rowSchema) ocs)
                    in "SELECT " <> schemaString rowSchema sc <> fromPart <> wherePart rowSchema wc <> orderPart
        tableSchema ::
            TupleTableSel tablesel row -> (SQLite.TableSchema (RowColSel row), Dict (IsSQLiteTable (RowColSel row)))
        tableSchema (MkTupleTableSel tsel) =
            case witnessConstraint @_ @IsSQLiteTable tsel of
                Dict -> (finiteGetAllFor databaseTables tsel, Dict)
        rowSchemaString ::
            WitnessConstraint ToField colsel => FiniteAllFor ColumnRefSchema colsel -> AllOf colsel -> QueryString
        rowSchemaString MkFiniteAllFor{..} (MkAllOf row) =
            "("
                <> intercalate
                    ","
                    ( fmap
                        ( \(MkSome col) ->
                            case witnessConstraint @_ @ToField col of
                                Dict -> valQueryString $ row col
                        )
                        finiteDomain
                    )
                <> ")"
        assignmentPart :: FiniteAllFor ColumnRefSchema colsel -> TupleUpdateItem SQLiteDatabase colsel -> QueryString
        assignmentPart scsh (MkTupleUpdateItem col expr) =
            (fromString $ columnRefName $ finiteGetAllFor scsh col) <> "=" <> schemaString scsh expr
        sqliteEditQuery :: SQLiteEdit tablesel -> QueryString
        sqliteEditQuery (DatabaseInsert (tableSchema -> (SQLite.MkTableSchema{..}, Dict)) (MkTupleInsertClause ic)) = let
            tableColumnRefs = mapFiniteAllFor (columnRef "") tableColumns
            in "INSERT OR REPLACE INTO "
                <> fromString tableName
                <> " VALUES "
                <> intercalate "," (fmap (rowSchemaString tableColumnRefs) ic)
        sqliteEditQuery (DatabaseDelete (tableSchema -> (SQLite.MkTableSchema{..}, _)) wc) = let
            tableColumnRefs = mapFiniteAllFor (columnRef "") tableColumns
            in "DELETE FROM " <> fromString tableName <> wherePart tableColumnRefs wc
        sqliteEditQuery (DatabaseUpdate (tableSchema -> (SQLite.MkTableSchema{..}, _)) wc (MkTupleUpdateClause uis)) = let
            tableColumnRefs = mapFiniteAllFor (columnRef "") tableColumns
            in "UPDATE "
                <> fromString tableName
                <> " SET "
                <> intercalate "," (fmap (assignmentPart tableColumnRefs) uis)
                <> wherePart tableColumnRefs wc
        refRead :: Readable (ReaderT Connection IO) (SQLiteReader tablesel)
        refRead r@(DatabaseSelect _ _ _ (MkTupleSelectClause _)) =
            case sqliteReadQuery r of
                MkQueryString s v -> do
                    conn <- ask
                    lift $ query conn s v
        refEdit ::
            NonEmpty (SQLiteEdit tablesel) -> ReaderT Connection IO (Maybe (EditSource -> ReaderT Connection IO ()))
        refEdit =
            alwaysEdit $ \edits _ -> let
                queries :: [QueryString]
                queries = fmap sqliteEditQuery $ toList edits
                pairs :: [(Query, NonEmpty [SQLData])]
                pairs = mapToList $ groupQueryStringsMap queries
                in for_ pairs $ \(s, vv) -> do
                    conn <- ask
                    lift $ executeMany conn s (toList vv)
        refCommitTask = mempty
    return $ MkResource objRun MkAReference{..}

{-# OPTIONS -fno-warn-orphans #-}

module Truth.World.SQLite
    ( module Truth.World.SQLite
    , SQLData
    , FromField(..)
    , ToField(..)
    ) where

import Database.SQLite.Simple hiding (columnName)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import System.Directory
import Truth.Core
import Truth.Core.Import
import qualified Truth.World.SQLite.Schema as SQLite

data QueryString =
    MkQueryString Query
                  [SQLData]

instance Semigroup QueryString where
    (MkQueryString qa va) <> (MkQueryString qb vb) = MkQueryString (mappend qa qb) (va <> vb)

instance Monoid QueryString where
    mempty = MkQueryString mempty mempty
    mappend = (<>)

instance IsString QueryString where
    fromString s = MkQueryString (fromString s) []

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

type instance RowColSel (AllValue colsel) = colsel

class HasSchema (t :: Type) where
    type Schema t :: Type
    schemaString :: Schema t -> t -> QueryString

data Expr colsel t where
    ConstExpr :: ToField t => t -> Expr colsel t
    ColumnExpr :: colsel t -> Expr colsel t
    EqualsExpr :: Eq t => Expr colsel t -> Expr colsel t -> Expr colsel Bool
    AndExpr :: Expr colsel Bool -> Expr colsel Bool -> Expr colsel Bool

instance AllWitnessConstraint Show colsel => Show (Expr colsel t) where
    show (ConstExpr t) = show $ toField t
    show (ColumnExpr col) = showAllWitness col
    show (EqualsExpr ea eb) = "(" ++ show ea ++ "=" ++ show eb ++ ")"
    show (AndExpr ea eb) = "(" ++ show ea ++ " & " ++ show eb ++ ")"

instance MeetSemiLattice (Expr colsel Bool) where
    (/\) = AndExpr

instance BoundedMeetSemiLattice (Expr colsel Bool) where
    top = ConstExpr True

class ExprEquals expr where
    (===) :: Eq t => expr t -> expr t -> expr Bool

instance ExprEquals (Expr colsel) where
    (===) = EqualsExpr

evalExpr :: Applicative m => Expr colsel t -> (forall a. colsel a -> m a) -> m t
evalExpr (ConstExpr v) _ = pure v
evalExpr (ColumnExpr sel) tuple = tuple sel
evalExpr (EqualsExpr e1 e2) tuple = (==) <$> evalExpr e1 tuple <*> evalExpr e2 tuple
evalExpr (AndExpr e1 e2) tuple = (&&) <$> evalExpr e1 tuple <*> evalExpr e2 tuple

data ColumnRefSchema t = MkColumnRefSchema
    { columnRefName :: String
    , columnRefType :: SQLite.ColumnTypeSchema t
    }

instance HasSchema (Expr colsel t) where
    type Schema (Expr colsel t) = SubmapWitness colsel ColumnRefSchema
    schemaString _ (ConstExpr t) = valQueryString t
    schemaString csch (ColumnExpr col) = fromString $ columnRefName $ subWitnessMap csch col
    schemaString csch (EqualsExpr e1 e2) = "(" <> schemaString csch e1 <> "=" <> schemaString csch e2 <> ")"
    schemaString csch (AndExpr e1 e2) = "(" <> schemaString csch e1 <> " AND " <> schemaString csch e2 <> ")"

class (FiniteWitness colsel, WitnessConstraint FromField colsel, WitnessConstraint ToField colsel) =>
          IsSQLiteTable colsel

instance (FiniteWitness colsel, WitnessConstraint FromField colsel, WitnessConstraint ToField colsel) =>
             IsSQLiteTable colsel

instance TupleDatabaseType SQLiteDatabase where
    type TupleDatabaseTypeRowWitness SQLiteDatabase = IsSQLiteTable
    type TupleExpr SQLiteDatabase colsel = Expr colsel
    evalTupleExpr expr (MkAllF tuple) = evalExpr expr tuple
    constBoolExpr = ConstExpr
    columnExpr = ColumnExpr

instance AllWitnessConstraint Show colsel => AllWitnessConstraint Show (Expr colsel) where
    allWitnessConstraint = Dict

instance ShowableTupleDatabaseType SQLiteDatabase where
    witnessShowTupleExpr = Dict

instance (FiniteWitness colsel, WitnessConstraint FromField colsel) => FromRow (AllValue colsel) where
    fromRow =
        assembleWitness $ \wt ->
            case witnessConstraint @Type @FromField wt of
                Dict -> fmap fromOnly fromRow

instance HasSchema (TupleWhereClause SQLiteDatabase row) where
    type Schema (TupleWhereClause SQLiteDatabase row) = SubmapWitness (RowColSel row) ColumnRefSchema
    schemaString csch (MkTupleWhereClause expr) = schemaString csch expr

intercalate' :: Monoid a => a -> [a] -> a
intercalate' _ [] = mempty
intercalate' _ [a] = a
intercalate' i (a:aa) = mconcat [a, i, intercalate' i aa]

instance HasSchema (TupleSelectClause SQLiteDatabase tablesel row row') where
    type Schema (TupleSelectClause SQLiteDatabase tablesel row row') = SubmapWitness (RowColSel row) ColumnRefSchema
    schemaString csch (MkTupleSelectClause mapSel) =
        intercalate' "," $ fmap (\(MkAnyW cs') -> schemaString csch $ mapSel cs') $ allWitnesses @(RowColSel row')

instance HasSchema (TupleOrderItem colsel) where
    type Schema (TupleOrderItem colsel) = SubmapWitness colsel ColumnRefSchema
    schemaString csch (MkTupleOrderItem col dir) =
        fromString $ (columnRefName $ subWitnessMap csch col) ++ " " ++ show dir

columnRef :: String -> SQLite.ColumnSchema t -> ColumnRefSchema t
columnRef tableRefName SQLite.MkColumnSchema {..} = let
    columnRefName =
        case tableRefName of
            "" -> columnName
            _ -> tableRefName ++ "." ++ columnName
    columnRefType = columnType
    in MkColumnRefSchema {..}

joinTableSchema ::
       forall tablesel row.
       SubmapWitness tablesel SQLite.TableSchema
    -> TableJoin SQLiteDatabase (TupleTableSel tablesel) row
    -> State Int ([QueryString], SubmapWitness (RowColSel row) ColumnRefSchema)
joinTableSchema schema (SingleTable (MkTupleTableSel tsel)) = do
    i <- get
    put $ i + 1
    let
        tableRefName = "t" ++ show i
        SQLite.MkTableSchema {..} = subWitnessMap schema tsel
        tabRefText = fromString $ tableName ++ " AS " ++ tableRefName
        colRefSchema = mapSubmapWitness (columnRef tableRefName) tableColumns
    return ([tabRefText], colRefSchema)
joinTableSchema schema (JoinTables OuterTupleJoinClause j1 j2) = do
    (t1, s1) <- joinTableSchema schema j1
    (t2, s2) <- joinTableSchema schema j2
    return $ (t1 ++ t2, eitherSubmapWitness s1 s2)

sqliteObject ::
       forall tablesel. WitnessConstraint IsSQLiteTable tablesel
    => FilePath
    -> SQLite.DatabaseSchema tablesel
    -> Object (SQLiteEdit tablesel)
sqliteObject path schema@SQLite.MkDatabaseSchema {..} = let
    objRun :: UnliftIO (ReaderT Connection IO)
    objRun =
        MkTransform $ \call -> do
            exists <- doesFileExist path
            withConnection path $ \conn -> do
                if exists
                    then return ()
                    else for_ (SQLite.toSchema schema) $ execute_ conn -- create the database if we're creating the file
                runReaderT call conn
    wherePart :: Schema (TupleWhereClause SQLiteDatabase row) -> TupleWhereClause SQLiteDatabase row -> QueryString
    wherePart rowSchema wc =
        case wc of
            MkTupleWhereClause (ConstExpr True) -> ""
            _ -> " WHERE " <> schemaString rowSchema wc
    sqliteReadQuery :: SQLiteReader tablesel [AllValue colsel] -> QueryString
    sqliteReadQuery (DatabaseSelect jc wc oc sc) =
        case evalState (joinTableSchema databaseTables jc) 1 of
            (tabRefs, rowSchema) -> let
                fromPart :: QueryString
                fromPart =
                    case tabRefs of
                        [] -> ""
                        _ -> " FROM " <> intercalate' "," tabRefs
                orderPart :: QueryString
                orderPart =
                    case oc of
                        MkTupleOrderClause [] -> ""
                        MkTupleOrderClause ocs -> " ORDER BY " <> (intercalate' "," $ fmap (schemaString rowSchema) ocs)
                in "SELECT " <> schemaString rowSchema sc <> fromPart <> wherePart rowSchema wc <> orderPart
    tableSchema ::
           TupleTableSel tablesel row -> (SQLite.TableSchema (RowColSel row), Dict (IsSQLiteTable (RowColSel row)))
    tableSchema (MkTupleTableSel tsel) =
        case witnessConstraint @_ @IsSQLiteTable tsel of
            Dict -> (subWitnessMap databaseTables tsel, Dict)
    rowSchemaString ::
           WitnessConstraint ToField colsel => SubmapWitness colsel ColumnRefSchema -> AllValue colsel -> QueryString
    rowSchemaString MkSubmapWitness {..} (MkAllValue row) =
        "(" <>
        intercalate'
            ","
            (fmap
                 (\(MkAnyW col) ->
                      case witnessConstraint @_ @ToField col of
                          Dict -> valQueryString $ row col)
                 subWitnessDomain) <>
        ")"
    assignmentPart :: SubmapWitness colsel ColumnRefSchema -> TupleUpdateItem SQLiteDatabase colsel -> QueryString
    assignmentPart scsh (MkTupleUpdateItem col expr) =
        (fromString $ columnRefName $ subWitnessMap scsh col) <> "=" <> schemaString scsh expr
    sqliteEditQuery :: SQLiteEdit tablesel -> QueryString
    sqliteEditQuery (DatabaseInsert (tableSchema -> (SQLite.MkTableSchema {..}, Dict)) (MkTupleInsertClause ic)) = let
        tableColumnRefs = mapSubmapWitness (columnRef "") tableColumns
        in "INSERT OR REPLACE INTO " <>
           fromString tableName <> " VALUES " <> intercalate' "," (fmap (rowSchemaString tableColumnRefs) ic)
    sqliteEditQuery (DatabaseDelete (tableSchema -> (SQLite.MkTableSchema {..}, _)) wc) = let
        tableColumnRefs = mapSubmapWitness (columnRef "") tableColumns
        in "DELETE FROM " <> fromString tableName <> wherePart tableColumnRefs wc
    sqliteEditQuery (DatabaseUpdate (tableSchema -> (SQLite.MkTableSchema {..}, _)) wc (MkTupleUpdateClause uis)) = let
        tableColumnRefs = mapSubmapWitness (columnRef "") tableColumns
        in "UPDATE " <>
           fromString tableName <>
           " SET " <> intercalate' "," (fmap (assignmentPart tableColumnRefs) uis) <> wherePart tableColumnRefs wc
    objRead :: MutableRead (ReaderT Connection IO) (SQLiteReader tablesel)
    objRead r@(DatabaseSelect _ _ _ (MkTupleSelectClause _)) =
        case sqliteReadQuery r of
            MkQueryString s v -> do
                conn <- ask
                lift $ query conn s v
    objEdit :: [SQLiteEdit tablesel] -> ReaderT Connection IO (Maybe (EditSource -> ReaderT Connection IO ()))
    objEdit =
        singleAlwaysEdit $ \edit _ ->
            case sqliteEditQuery edit of
                MkQueryString s v -> do
                    conn <- ask
                    lift $ execute conn s v
    in MkCloseUnliftIO objRun MkAnObject {..}

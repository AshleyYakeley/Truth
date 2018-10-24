module Pinafore.Language.Read
    ( parseExpression
    , InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Data.UUID hiding (fromString)
import Pinafore.Entity
import Pinafore.Language.Entity
import Pinafore.Language.Expression
import Pinafore.Language.If
import Pinafore.Language.Literal
import Pinafore.Language.Morphism
import Pinafore.Language.Name
import Pinafore.Language.Token
import Pinafore.Language.Type
import Pinafore.PredicateMorphism
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

type Parser = Parsec [(SourcePos, Any Token)] ()

readThis :: Token t -> Parser t
readThis tok =
    token (\(_, MkAny tok' _) -> show tok') fst $ \(_, MkAny tok' t) ->
        case testEquality tok tok' of
            Just Refl -> Just t
            Nothing -> Nothing

readExactlyThis :: Eq t => Token t -> t -> Parser ()
readExactlyThis tok t = do
    a <- readThis tok
    if a == t
        then return ()
        else mzero

readBracketed :: Token () -> Token () -> Parser t -> Parser t
readBracketed op cl pp = do
    readThis op
    t <- pp
    readThis cl
    return t

readParen :: Parser t -> Parser t
readParen = readBracketed TokOpenParen TokCloseParen

readBracket :: Parser t -> Parser t
readBracket = readBracketed TokOpenBracket TokCloseBracket

readCommaList1 :: Semigroup t => Parser t -> Parser t
readCommaList1 p = do
    v1 <- p
    mv2 <-
        optional $ do
            readThis TokComma
            readCommaList1 p
    case mv2 of
        Just v2 -> return $ v1 <> v2
        Nothing -> return v1

readCommaList :: Monoid t => Parser t -> Parser t
readCommaList p = readCommaList1 p <|> return mempty

readJoinMeet ::
       forall polarity. IsTypePolarity polarity
    => Parser ()
readJoinMeet =
    case whichTypePolarity @polarity of
        Left _ -> readExactlyThis TokOperator "|"
        Right _ -> readExactlyThis TokOperator "&"

readType ::
       forall baseedit polarity. IsTypePolarity polarity
    => Parser (AnyWitness (PinaforeType baseedit polarity))
readType = do
    t1 <- readType1
    mt2 <-
        optional $ do
            readJoinMeet @polarity
            readType
    case mt2 of
        Just t2 -> return $ t1 <> t2
        Nothing -> return t1

readType1 ::
       forall baseedit polarity. IsTypePolarity polarity
    => Parser (AnyWitness (PinaforeType baseedit polarity))
readType1 =
    (try $ do
         MkAnyInKind t1 <- readTypeRange3
         readThis TokPropMap
         MkAnyInKind t2 <- readTypeRange3
         return $
             MkAnyWitness $
             singlePinaforeType $
             GroundPinaforeSingularType MorphismPinaforeGroundType $
             ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments) <|>
    (try $
     invertPolarity @polarity $ do
         MkAnyWitness t1 <- invertPolarity @polarity readType3
         readThis TokMap
         MkAnyWitness t2 <- readType1
         return $
             MkAnyWitness $
             singlePinaforeType $
             GroundPinaforeSingularType FuncPinaforeGroundType $
             ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments) <|>
    readType2

readType2 ::
       forall baseedit polarity. IsTypePolarity polarity
    => Parser (AnyWitness (PinaforeType baseedit polarity))
readType2 =
    (do
         readExactlyThis TokName "Ref"
         MkAnyInKind t <- readTypeRange3
         return $
             MkAnyWitness $
             singlePinaforeType $
             GroundPinaforeSingularType ReferencePinaforeGroundType $ ConsDolanArguments t NilDolanArguments) <|>
    (do
         readExactlyThis TokName "Set"
         MkAnyInKind t <- readTypeRange3
         return $
             MkAnyWitness $
             singlePinaforeType $
             GroundPinaforeSingularType SetPinaforeGroundType $ ConsDolanArguments t NilDolanArguments) <|>
    (do
         readExactlyThis TokName "Order"
         MkAnyWitness t <- invertPolarity @polarity readType3
         return $
             MkAnyWitness $
             singlePinaforeType $
             GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments t NilDolanArguments) <|>
    readType3

readType3 ::
       forall baseedit polarity. IsTypePolarity polarity
    => Parser (AnyWitness (PinaforeType baseedit polarity))
readType3 =
    (do
         MkAnyWitness t <- readBracket readType
         return $
             MkAnyWitness $
             singlePinaforeType $
             GroundPinaforeSingularType ListPinaforeGroundType $ ConsDolanArguments t NilDolanArguments) <|>
    (do
         MkAnyWitness s <- readTypeVar
         return $ MkAnyWitness $ singlePinaforeType $ VarPinaforeSingularType s) <|>
    readTypeConst <|>
    (readParen $ do
         mt <- optional readType
         case mt of
             Just (MkAnyWitness t) -> do
                 comma <- optional $ readThis TokComma
                 case comma of
                     Just () -> do
                         MkAnyWitness t2 <- readType
                         return $
                             MkAnyWitness $
                             singlePinaforeType $
                             GroundPinaforeSingularType PairPinaforeGroundType $
                             ConsDolanArguments t $ ConsDolanArguments t2 NilDolanArguments
                     Nothing -> return $ MkAnyWitness t
             Nothing -> return $ MkAnyWitness $ literalPinaforeType UnitLiteralType)

readTypeRange3 ::
       forall baseedit polarity. IsTypePolarity polarity
    => Parser (AnyInKind (TypeRangeWitness (PinaforeType baseedit) polarity))
readTypeRange3 = readBracketed TokOpenBrace TokCloseBrace $ readCommaList readTypeRangeItem

readTypeRangeItem ::
       forall baseedit polarity. IsTypePolarity polarity
    => Parser (AnyInKind (TypeRangeWitness (PinaforeType baseedit) polarity))
readTypeRangeItem =
    (do
         readExactlyThis TokOperator "+"
         MkAnyWitness tq <- readType3
         return $ MkAnyInKind $ MkTypeRangeWitness NilPinaforeType tq) <|>
    (do
         readExactlyThis TokOperator "-"
         MkAnyWitness tp <- invertPolarity @polarity readType3
         return $ MkAnyInKind $ MkTypeRangeWitness tp NilPinaforeType)

readTypeVar :: Parser (AnyWitness SymbolWitness)
readTypeVar = do
    s <- try $ readThis TokName
    case unpack s of
        c:_
            | isLower c -> return $ toSymbolWitness (unpack s) MkAnyWitness
        _ -> mzero

readTypeConst ::
       forall baseedit polarity. IsTypePolarity polarity
    => Parser (AnyWitness (PinaforeType baseedit polarity))
readTypeConst = do
    n <- readTypeName
    case n of
        "Any" ->
            case whichTypePolarity @polarity of
                Left Refl ->
                    return $
                    MkAnyWitness $
                    singlePinaforeType $ GroundPinaforeSingularType InvertLimitPinaforeGroundType NilDolanArguments
                Right Refl -> return $ MkAnyWitness NilPinaforeType
        "None" ->
            case whichTypePolarity @polarity of
                Left Refl -> return $ MkAnyWitness NilPinaforeType
                Right Refl ->
                    return $
                    MkAnyWitness $
                    singlePinaforeType $ GroundPinaforeSingularType InvertLimitPinaforeGroundType NilDolanArguments
        "Entity" ->
            return $
            MkAnyWitness $ singlePinaforeType $ GroundPinaforeSingularType EntityPinaforeGroundType NilDolanArguments
        "Point" ->
            return $
            MkAnyWitness $ singlePinaforeType $ GroundPinaforeSingularType PointPinaforeGroundType NilDolanArguments
        "Literal" -> return $ MkAnyWitness $ literalPinaforeType LiteralLiteralType
        "Text" -> return $ MkAnyWitness $ literalPinaforeType TextLiteralType
        "Number" -> return $ MkAnyWitness $ literalPinaforeType NumberLiteralType
        "Bool" -> return $ MkAnyWitness $ literalPinaforeType BooleanLiteralType
        "Action" ->
            return $
            MkAnyWitness $ singlePinaforeType $ GroundPinaforeSingularType ActionPinaforeGroundType NilDolanArguments
        "UI" ->
            return $
            MkAnyWitness $
            singlePinaforeType $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments
        _ -> fail $ "unknown type: " <> show n

readTypeName :: Parser Name
readTypeName = do
    s <- try $ readThis TokName
    case unpack s of
        c:_
            | isUpper c -> return s
        _ -> mzero

readPattern :: Parser Name
readPattern = readThis TokName

newtype TypeDecls =
    MkTypeDecls (forall a. PinaforeTypeCheck a -> PinaforeTypeCheck a)

instance Semigroup TypeDecls where
    (MkTypeDecls a) <> (MkTypeDecls b) = MkTypeDecls (a . b)

instance Monoid TypeDecls where
    mempty = MkTypeDecls id
    mappend = (<>)

type Declarations baseedit = (TypeDecls, QBindList baseedit)

readEntityDeclaration :: Parser (Declarations baseedit)
readEntityDeclaration = do
    readThis TokEntity
    n <- readThis TokName
    return (MkTypeDecls $ withNewTypeName n $ EntityNamedType $ toSymbolWitness (unpack n) MkAnyWitness, mempty)

readSubtypeDeclaration :: Parser (Declarations baseedit)
readSubtypeDeclaration = do
    readThis TokSubtype
    na <- readThis TokName
    readExactlyThis TokOperator "<="
    nb <- readThis TokName
    return (MkTypeDecls $ withEntitySubtype (na, nb), mempty)

readBinding :: HasPinaforeEntityEdit baseedit => Parser (Declarations baseedit)
readBinding = do
    name <- readThis TokName
    args <- many readPattern
    readThis TokAssign
    tval <- readExpression
    return
        ( mempty
        , qBindExpr name $ do
              val <- tval
              qAbstractsExpr args val)

readDeclaration :: HasPinaforeEntityEdit baseedit => Parser (Declarations baseedit)
readDeclaration = readEntityDeclaration <|> readSubtypeDeclaration <|> readBinding

readDeclarations :: HasPinaforeEntityEdit baseedit => Parser (Declarations baseedit)
readDeclarations =
    (do
         b <- readDeclaration
         mbl <-
             optional $ do
                 readThis TokSemicolon
                 readDeclarations
         return $ b <> fromMaybe mempty mbl) <|>
    (do return mempty)

readLetBindings :: HasPinaforeEntityEdit baseedit => Parser (QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit))
readLetBindings = do
    readThis TokLet
    (MkTypeDecls td, bl) <- readDeclarations
    f <- qBindingsLetExpr bl
    return $ \expr -> td $ f expr

readExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (PinaforeTypeCheck (QExpr baseedit))
readExpression = readInfixedExpression 0

data FixAssoc
    = AssocNone
    | AssocLeft
    | AssocRight
    deriving (Eq)

data Fixity =
    MkFixity FixAssoc
             Int

-- following Haskell
-- https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-820061
operatorFixity :: Name -> Fixity
operatorFixity "." = MkFixity AssocRight 9
operatorFixity "<.>" = MkFixity AssocRight 9
operatorFixity "*" = MkFixity AssocLeft 8
operatorFixity "/" = MkFixity AssocLeft 8
operatorFixity "/\\" = MkFixity AssocLeft 8
operatorFixity "+" = MkFixity AssocLeft 7
operatorFixity "-" = MkFixity AssocLeft 7
operatorFixity "\\/" = MkFixity AssocLeft 7
operatorFixity "++" = MkFixity AssocRight 6
operatorFixity "==" = MkFixity AssocNone 5
operatorFixity "/=" = MkFixity AssocNone 5
operatorFixity "~==" = MkFixity AssocNone 5
operatorFixity "~/=" = MkFixity AssocNone 5
operatorFixity "<=" = MkFixity AssocNone 5
operatorFixity "<" = MkFixity AssocNone 5
operatorFixity ">=" = MkFixity AssocNone 5
operatorFixity ">" = MkFixity AssocNone 5
operatorFixity "&&" = MkFixity AssocRight 4
operatorFixity "||" = MkFixity AssocRight 3
operatorFixity ":=" = MkFixity AssocNone 2
operatorFixity "+=" = MkFixity AssocNone 2
operatorFixity "-=" = MkFixity AssocNone 2
operatorFixity ">>" = MkFixity AssocLeft 1
operatorFixity "??" = MkFixity AssocLeft 1
operatorFixity "$" = MkFixity AssocRight 0
operatorFixity _ = MkFixity AssocLeft 9

readInfix :: Int -> Parser (FixAssoc, Name)
readInfix prec =
    Text.Parsec.try $ do
        name <- readThis TokOperator
        let MkFixity assoc fprec = operatorFixity name
        if prec == fprec
            then return (assoc, name)
            else empty

leftApply ::
       HasPinaforeEntityEdit baseedit
    => QExpr baseedit
    -> [(QExpr baseedit, QExpr baseedit)]
    -> PinaforeTypeCheck (QExpr baseedit)
leftApply e1 [] = return e1
leftApply e1 ((f, e2):rest) = do
    ee <- qApplyAllExpr f [e1, e2]
    leftApply ee rest

rightApply ::
       HasPinaforeEntityEdit baseedit
    => QExpr baseedit
    -> [(QExpr baseedit, QExpr baseedit)]
    -> PinaforeTypeCheck (QExpr baseedit)
rightApply e1 [] = return e1
rightApply e1 ((f, e2):rest) = do
    ee <- rightApply e2 rest
    qApplyAllExpr f [e1, ee]

readInfixedExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Int
    -> Parser (PinaforeTypeCheck (QExpr baseedit))
readInfixedExpression 10 = readExpression1
readInfixedExpression prec = do
    te1 <- readInfixedExpression (succ prec)
    rest <-
        many $ do
            (assoc, name) <- readInfix prec
            te2 <- readInfixedExpression (succ prec)
            return (assoc, name, te2)
    case rest of
        [] -> return te1
        [(AssocNone, name, te2)] ->
            return $ do
                e1 <- te1
                e2 <- te2
                qApplyAllExpr (qVarExpr name) [e1, e2]
        _
            | all (\(assoc, _, _) -> assoc == AssocLeft) rest ->
                return $ do
                    e1 <- te1
                    pairs <-
                        for rest $ \(_, name, te2) -> do
                            e2 <- te2
                            return (qVarExpr name, e2)
                    leftApply e1 pairs
        _
            | all (\(assoc, _, _) -> assoc == AssocRight) rest ->
                return $ do
                    e1 <- te1
                    pairs <-
                        for rest $ \(_, name, te2) -> do
                            e2 <- te2
                            return (qVarExpr name, e2)
                    rightApply e1 pairs
        _ -> parserFail $ "incompatible infix operators: " ++ intercalate " " (fmap (\(_, name, _) -> show name) rest)

readExpression1 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (PinaforeTypeCheck (QExpr baseedit))
readExpression1 =
    (do
         readThis TokLambda
         args <- many readPattern
         readThis TokMap
         mval <- readExpression
         return $ do
             val <- mval
             qAbstractsExpr args val) <|>
    (do
         bmap <- readLetBindings
         readThis TokIn
         mbody <- readExpression
         return $ do
             body <- mbody
             bmap body) <|>
    (do
         readThis TokIf
         metest <- readExpression
         readThis TokThen
         methen <- readExpression
         readThis TokElse
         meelse <- readExpression
         return $ do
             etest <- metest
             ethen <- methen
             eelse <- meelse
             qApplyAllExpr (qConstExpr qifthenelse) [etest, ethen, eelse]) <|>
    readExpression2

readExpression2 :: HasPinaforeEntityEdit baseedit => Parser (PinaforeTypeCheck (QExpr baseedit))
readExpression2 = do
    te1 <- readExpression3
    targs <- many readExpression3
    return $ do
        e1 <- te1
        args <- sequence targs
        qApplyAllExpr e1 args

makePositivePointConversion ::
       forall m baseedit polarity t. (MonadFail m, IsTypePolarity polarity)
    => PinaforeType baseedit polarity t
    -> m (Point -> t)
makePositivePointConversion (ConsPinaforeType (GroundPinaforeSingularType (NamedEntityPinaforeGroundType _) NilDolanArguments) NilPinaforeType) =
    return $
    case whichTypePolarity @polarity of
        Left Refl -> join1 . MkNamedEntity
        Right Refl -> meetf MkNamedEntity alwaysTop
makePositivePointConversion tp = fail $ "not an entity type: " <> show tp

makeNegativePointConversion ::
       forall m baseedit polarity t. (MonadFail m, IsTypePolarity polarity)
    => PinaforeType baseedit polarity t
    -> m (t -> Point)
makeNegativePointConversion (ConsPinaforeType (GroundPinaforeSingularType (NamedEntityPinaforeGroundType _) NilDolanArguments) NilPinaforeType) =
    return $
    case whichTypePolarity @polarity of
        Left Refl -> joinf unNamedEntity never
        Right Refl -> unNamedEntity . meet1
makeNegativePointConversion tp = fail $ "not an entity type: " <> show tp

makePointTypeRange ::
       forall m baseedit polarity pq. (MonadFail m, IsTypePolarity polarity)
    => TypeRangeWitness (PinaforeType baseedit) polarity pq
    -> m (TypeRange Point pq)
makePointTypeRange (MkTypeRangeWitness tp tq) = do
    convp <- invertPolarity @polarity $ makeNegativePointConversion tp
    convq <- makePositivePointConversion tq
    return $ MkTypeRange convp convq

makeProperty ::
       (MonadFail m, HasPinaforeEntityEdit baseedit) => PinaforeType baseedit 'PositivePolarity t -> UUID -> m t
makeProperty (ConsPinaforeType (GroundPinaforeSingularType MorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments))) NilPinaforeType) uuid = do
    tra <- makePointTypeRange ta
    trb <- makePointTypeRange tb
    return $ LeftJoinType $ MkPinaforeMorphism tra trb $ predicatePinaforeLensMorphism $ MkPredicate uuid
makeProperty tp _ = fail $ "not an property type: " <> show tp

makePoint :: MonadFail m => PinaforeType baseedit 'PositivePolarity t -> UUID -> m t
makePoint (ConsPinaforeType (GroundPinaforeSingularType (NamedEntityPinaforeGroundType _) NilDolanArguments) NilPinaforeType) uuid =
    return $ LeftJoinType $ MkNamedEntity $ MkPoint uuid
makePoint tp _ = fail $ "not an entity type: " <> show tp

readExpression3 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (PinaforeTypeCheck (QExpr baseedit))
readExpression3 =
    (do
         b <- readThis TokBool
         return $ return $ qConstExpr b) <|>
    (do
         name <- readThis TokName
         return $ return $ qVarExpr name) <|>
    (do
         n <- readThis TokNumber
         return $ return $ qConstExpr n) <|>
    (do
         str <- readThis TokString
         return $ return $ qConstExpr str) <|>
    (do
         readThis TokProperty
         readThis TokAt
         MkAnyWitness tp <- readType3
         uuid <- readThis TokUUID
         prop <- makeProperty tp uuid
         return $ return $ qConstExprAny $ MkAny tp prop) <|>
    (do
         readThis TokPoint
         readThis TokAt
         MkAnyWitness tp <- readType3
         uuid <- readThis TokUUID
         pt <- makePoint tp uuid
         return $ return $ qConstExprAny $ MkAny tp pt) <|>
    (readParen readExpression) <|>
    (do
         readThis TokOpenBracket
         mexprs <-
             (do
                  expr1 <- readExpression
                  exprs <-
                      many $ do
                          readThis TokComma
                          readExpression
                  return $ expr1 : exprs) <|>
             return []
         readThis TokCloseBracket
         return $ do
             exprs <- sequence mexprs
             qSequenceExpr exprs) <?>
    "expression"

data InteractiveCommand baseedit
    = LetInteractiveCommand (QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit))
    | ExpressionInteractiveCommand (PinaforeTypeCheck (QExpr baseedit))

readInteractiveCommand ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (InteractiveCommand baseedit)
readInteractiveCommand =
    (eof >> return (LetInteractiveCommand return)) <|> (try $ fmap ExpressionInteractiveCommand readExpression) <|>
    (fmap LetInteractiveCommand readLetBindings)

parseReader :: Parser t -> SourceName -> Text -> Result Text t
parseReader parser name text = do
    toks <- parseTokens name text
    case parse parser name toks of
        Right a -> return a
        Left e -> fail $ show e

parseExpression ::
       HasPinaforeEntityEdit baseedit => SourceName -> Text -> Result Text (PinaforeTypeCheck (QExpr baseedit))
parseExpression = parseReader readExpression

parseInteractiveCommand ::
       HasPinaforeEntityEdit baseedit => SourceName -> Text -> Result Text (InteractiveCommand baseedit)
parseInteractiveCommand = parseReader readInteractiveCommand

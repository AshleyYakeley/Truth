module Pinafore.Language.Read
    ( parseExpression
    , InteractiveCommand(..)
    , parseInteractiveCommand
    ) where

import Data.UUID hiding (fromString)
import Language.Expression.Dolan.MPolarity
import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.If
import Pinafore.Language.Literal
import Pinafore.Language.Morphism
import Pinafore.Language.Name
import Pinafore.Language.NamedEntity
import Pinafore.Language.Token
import Pinafore.Language.Type
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

type Parser = Parsec [(SourcePos, AnyValue Token)] ()

readThis :: Token t -> Parser t
readThis tok =
    token (\(_, MkAnyValue tok' _) -> show tok') fst $ \(_, MkAnyValue tok' t) ->
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

type PinaforeType3 baseedit = MPolarType (PinaforeType baseedit)

type PinaforeTypeRange3 baseedit = MPolarTypeRange (PinaforeType baseedit)

readJoinMeet ::
       forall mpolarity. Is MPolarity mpolarity
    => Parser ()
readJoinMeet =
    case representative @_ @MPolarity @mpolarity of
        PositiveMPolarity -> readExactlyThis TokOperator "|"
        NegativeMPolarity -> readExactlyThis TokOperator "&"
        BothMPolarity -> mempty

readType ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeType3 baseedit mpolarity)
readType = do
    t1 <- readType1
    mt2 <-
        optional $ do
            readJoinMeet @mpolarity
            readType
    case mt2 of
        Just t2 -> return $ toMPolar (<>) t1 t2
        Nothing -> return t1

readType1 ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeType3 baseedit mpolarity)
readType1 =
    (try $ do
         at1 <- readTypeRange3
         readThis TokPropMap
         at2 <- readTypeRange3
         return $
             toMPolar
                 (\(MkAnyInKind t1) (MkAnyInKind t2) ->
                      MkAnyW $
                      singlePinaforeType $
                      GroundPinaforeSingularType MorphismPinaforeGroundType $
                      ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments)
                 at1
                 at2) <|>
    (try $ do
         at1 <- invertMPolarity @mpolarity readType3
         readThis TokMap
         at2 <- readType1
         return $
             toMPolar
                 (\(MkAnyW t1) (MkAnyW t2) ->
                      MkAnyW $
                      singlePinaforeType $
                      GroundPinaforeSingularType FuncPinaforeGroundType $
                      ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments)
                 (MkInvertMPolarType at1)
                 (at2)) <|>
    readType2

readType2 ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeType3 baseedit mpolarity)
readType2 =
    (do
         readExactlyThis TokName "Ref"
         at1 <- readTypeRange3
         return $
             toMPolar
                 (\(MkAnyInKind t1) ->
                      MkAnyW $
                      singlePinaforeType $
                      GroundPinaforeSingularType ReferencePinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
                 at1) <|>
    (do
         readExactlyThis TokName "Set"
         at1 <- readTypeRange3
         return $
             toMPolar
                 (\(MkAnyInKind t1) ->
                      MkAnyW $
                      singlePinaforeType $
                      GroundPinaforeSingularType SetPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
                 at1) <|>
    (do
         readExactlyThis TokName "Order"
         at1 <- invertMPolarity @mpolarity readType3
         return $
             toMPolar
                 (\(MkAnyW t1) ->
                      MkAnyW $
                      singlePinaforeType $
                      GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
                 (MkInvertMPolarType at1)) <|>
    readType3

readType3 ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeType3 baseedit mpolarity)
readType3 =
    (do
         at1 <- readBracket $ readType
         return $
             toMPolar
                 (\(MkAnyW t1) ->
                      MkAnyW $
                      singlePinaforeType $
                      GroundPinaforeSingularType ListPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments)
                 at1) <|>
    (do
         MkAnyW s <- readTypeVar
         return $ toMPolar $ MkAnyW $ singlePinaforeType $ VarPinaforeSingularType s) <|>
    readTypeConst <|>
    (readParen $ do
         mt <- optional $ readType
         case mt of
             Just at1 -> do
                 comma <- optional $ readThis TokComma
                 case comma of
                     Just () -> do
                         at2 <- readType
                         return $
                             toMPolar
                                 (\(MkAnyW t1) (MkAnyW t2) ->
                                      MkAnyW $
                                      singlePinaforeType $
                                      GroundPinaforeSingularType PairPinaforeGroundType $
                                      ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments)
                                 at1
                                 at2
                     Nothing -> return at1
             Nothing -> return $ toMPolar $ MkAnyW $ literalPinaforeType UnitLiteralType)

readTypeRange3 ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeTypeRange3 baseedit mpolarity)
readTypeRange3 = readBracketed TokOpenBrace TokCloseBrace $ readCommaList readTypeRangeItem <|> readTypeRangeItem

readTypeRangeItem ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeTypeRange3 baseedit mpolarity)
readTypeRangeItem =
    (do
         readExactlyThis TokOperator "+"
         atq <- readType3
         return $ toMPolar (\(MkAnyW tq) -> MkAnyInKind $ MkTypeRangeWitness NilPinaforeType tq) atq) <|>
    (do
         readExactlyThis TokOperator "-"
         atp <- invertMPolarity @mpolarity readType3
         return $
             toMPolar (\(MkAnyW tp) -> MkAnyInKind $ MkTypeRangeWitness tp NilPinaforeType) (MkInvertMPolarType atp)) <|>
    (do
         BothMPolarType atw <- readType3 @baseedit @'Nothing
         let
             ff :: forall polarity. IsTypePolarity polarity
                => AnyInKind (TypeRangeWitness (PinaforeType baseedit) polarity)
             ff =
                 case (invertPolarity @polarity $ atw @(InvertPolarity polarity), atw @polarity) of
                     (MkAnyW tp, MkAnyW tq) -> MkAnyInKind $ MkTypeRangeWitness tp tq
         return $ toMPolar ff)

readTypeVar :: Parser (AnyW SymbolWitness)
readTypeVar = do
    s <- try $ readThis TokName
    case unpack s of
        c:_
            | isLower c -> return $ toSymbolWitness (unpack s) MkAnyW
        _ -> mzero

readTypeConst ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeType3 baseedit mpolarity)
readTypeConst = do
    n <- readTypeName
    case n of
        "Any" ->
            return $
            toMPolar $ let
                t :: forall polarity. IsTypePolarity polarity
                  => AnyW (PinaforeType baseedit polarity)
                t =
                    case whichTypePolarity @polarity of
                        Left Refl ->
                            MkAnyW $
                            singlePinaforeType $
                            GroundPinaforeSingularType InvertLimitPinaforeGroundType NilDolanArguments
                        Right Refl -> MkAnyW NilPinaforeType
                in t
        "None" ->
            return $
            toMPolar $ let
                t :: forall polarity. IsTypePolarity polarity
                  => AnyW (PinaforeType baseedit polarity)
                t =
                    case whichTypePolarity @polarity of
                        Left Refl -> MkAnyW NilPinaforeType
                        Right Refl ->
                            MkAnyW $
                            singlePinaforeType $
                            GroundPinaforeSingularType InvertLimitPinaforeGroundType NilDolanArguments
                in t
        "Entity" ->
            return $
            toMPolar $
            MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType EntityPinaforeGroundType NilDolanArguments
        "Point" ->
            return $
            toMPolar $
            MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType PointPinaforeGroundType NilDolanArguments
        "Literal" -> return $ toMPolar $ MkAnyW $ literalPinaforeType LiteralLiteralType
        "Text" -> return $ toMPolar $ MkAnyW $ literalPinaforeType TextLiteralType
        "Number" -> return $ toMPolar $ MkAnyW $ literalPinaforeType NumberLiteralType
        "Bool" -> return $ toMPolar $ MkAnyW $ literalPinaforeType BooleanLiteralType
        "Action" ->
            return $
            toMPolar $
            MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType ActionPinaforeGroundType NilDolanArguments
        "UI" ->
            return $
            toMPolar $
            MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments
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
    return (MkTypeDecls $ withNewTypeName n $ EntityNamedType $ toSymbolWitness (unpack n) MkAnyW, mempty)

readSubtypeDeclaration :: Parser (Declarations baseedit)
readSubtypeDeclaration = do
    readThis TokSubtype
    na <- readThis TokName
    readExactlyThis TokOperator "<="
    nb <- readThis TokName
    return (MkTypeDecls $ withEntitySubtype (na, nb), mempty)

readBinding :: HasPinaforePointEdit baseedit => Parser (Declarations baseedit)
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

readDeclaration :: HasPinaforePointEdit baseedit => Parser (Declarations baseedit)
readDeclaration = readEntityDeclaration <|> readSubtypeDeclaration <|> readBinding

readDeclarations :: HasPinaforePointEdit baseedit => Parser (Declarations baseedit)
readDeclarations =
    (do
         b <- readDeclaration
         mbl <-
             optional $ do
                 readThis TokSemicolon
                 readDeclarations
         return $ b <> fromMaybe mempty mbl) <|>
    (do return mempty)

readLetBindings :: HasPinaforePointEdit baseedit => Parser (QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit))
readLetBindings = do
    readThis TokLet
    (MkTypeDecls td, bl) <- readDeclarations
    f <- qBindingsLetExpr bl
    return $ \expr -> td $ f expr

readExpression ::
       forall baseedit. HasPinaforePointEdit baseedit
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
       HasPinaforePointEdit baseedit
    => QExpr baseedit
    -> [(QExpr baseedit, QExpr baseedit)]
    -> PinaforeTypeCheck (QExpr baseedit)
leftApply e1 [] = return e1
leftApply e1 ((f, e2):rest) = do
    ee <- qApplyAllExpr f [e1, e2]
    leftApply ee rest

rightApply ::
       HasPinaforePointEdit baseedit
    => QExpr baseedit
    -> [(QExpr baseedit, QExpr baseedit)]
    -> PinaforeTypeCheck (QExpr baseedit)
rightApply e1 [] = return e1
rightApply e1 ((f, e2):rest) = do
    ee <- rightApply e2 rest
    qApplyAllExpr f [e1, ee]

readInfixedExpression ::
       forall baseedit. HasPinaforePointEdit baseedit
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
       forall baseedit. HasPinaforePointEdit baseedit
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

readExpression2 :: HasPinaforePointEdit baseedit => Parser (PinaforeTypeCheck (QExpr baseedit))
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

makeProperty :: (MonadFail m, HasPinaforePointEdit baseedit) => PinaforeType baseedit 'PositivePolarity t -> UUID -> m t
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
       forall baseedit. HasPinaforePointEdit baseedit
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
         SingleMPolarType (MkAnyW tp) <- readType3 @baseedit @('Just 'PositivePolarity)
         uuid <- readThis TokUUID
         prop <- makeProperty tp uuid
         return $ return $ qConstExprAny $ MkAnyValue tp prop) <|>
    (do
         readThis TokPoint
         readThis TokAt
         SingleMPolarType (MkAnyW tp) <- readType3 @baseedit @('Just 'PositivePolarity)
         uuid <- readThis TokUUID
         pt <- makePoint tp uuid
         return $ return $ qConstExprAny $ MkAnyValue tp pt) <|>
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
    | ShowTypeInteractiveCommand (PinaforeTypeCheck (QExpr baseedit))
    | ErrorInteractiveCommand Text

showTypeInteractiveCommand ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (InteractiveCommand baseedit)
showTypeInteractiveCommand = do
    expr <- readExpression
    return $ ShowTypeInteractiveCommand expr

readInteractiveCommand ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (InteractiveCommand baseedit)
readInteractiveCommand =
    (do
         readExactlyThis TokOperator ":"
         MkName cmd <- readThis TokName
         case cmd of
             "t" -> showTypeInteractiveCommand
             "type" -> showTypeInteractiveCommand
             _ -> return $ ErrorInteractiveCommand $ "unknown interactive command: " <> cmd) <|>
    (eof >> return (LetInteractiveCommand return)) <|>
    (try $ fmap ExpressionInteractiveCommand readExpression) <|>
    (fmap LetInteractiveCommand readLetBindings)

parseReader :: Parser t -> SourceName -> Text -> Result Text t
parseReader parser name text = do
    toks <- parseTokens name text
    case parse parser name toks of
        Right a -> return a
        Left e -> fail $ show e

parseExpression ::
       HasPinaforePointEdit baseedit => SourceName -> Text -> Result Text (PinaforeTypeCheck (QExpr baseedit))
parseExpression = parseReader readExpression

parseInteractiveCommand ::
       HasPinaforePointEdit baseedit => SourceName -> Text -> Result Text (InteractiveCommand baseedit)
parseInteractiveCommand = parseReader readInteractiveCommand

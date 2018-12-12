module Pinafore.Language.Read.Expression
    ( readTopExpression
    , readTopLetBindings
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.If
import Pinafore.Language.Name
import Pinafore.Language.NamedEntity
import Pinafore.Language.Read.Infix
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Property
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Read.TypeDecls
import Pinafore.Language.Show
import Pinafore.Language.Type
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

readPattern :: Parser Name
readPattern = readThis TokName

data Declarations baseedit =
    MkDeclarations (TypeDecls baseedit)
                   (RefNotation baseedit (QBindings baseedit))

instance Semigroup (Declarations baseedit) where
    (MkDeclarations ta ba) <> (MkDeclarations tb bb) = MkDeclarations (ta <> tb) (liftA2 (<>) ba bb)

instance Monoid (Declarations baseedit) where
    mempty = MkDeclarations mempty $ return mempty
    mappend = (<>)

typeDeclarations :: TypeDecls baseedit -> Declarations baseedit
typeDeclarations td = MkDeclarations td $ return mempty

readBinding :: HasPinaforeEntityEdit baseedit => Parser (PinaforeScoped baseedit (Declarations baseedit))
readBinding = do
    spos <- getPosition
    name <- readThis TokName
    args <- many readPattern
    readThis TokAssign
    rval <- readExpression
    return $
        return $
        MkDeclarations mempty $ do
            val <- rval
            expr <- liftRefNotation $ runSourcePos spos $ qAbstractsExpr args val
            return $ qBindExpr name expr

readDeclaration :: HasPinaforeEntityEdit baseedit => Parser (PinaforeScoped baseedit (Declarations baseedit))
readDeclaration = fmap (fmap typeDeclarations) readTypeDeclaration <|> readBinding

readDeclarations :: HasPinaforeEntityEdit baseedit => Parser (PinaforeScoped baseedit (Declarations baseedit))
readDeclarations =
    (do
         b <- readDeclaration
         mbl <-
             optional $ do
                 readThis TokSemicolon
                 readDeclarations
         return $ b <> fromMaybe mempty mbl) <|>
    (do return mempty)

readLetBindings ::
       HasPinaforeEntityEdit baseedit
    => Parser (PinaforeScoped baseedit (Transform (RefNotation baseedit) (RefNotation baseedit)))
readLetBindings = do
    spos <- getPosition
    readThis TokLet
    sdecl <- readDeclarations
    return $ do
        MkDeclarations (MkTypeDecls td) rbl <- sdecl
        return $
            MkTransform $ \ra ->
                td $ do
                    bl <- rbl
                    f <- qBindingsLetExpr bl
                    remonadRefNotation
                        (\se -> do
                             bmap <- runSourcePos spos f
                             withNewBindings bmap se) $
                        td ra

readTopLetBindings ::
       HasPinaforeEntityEdit baseedit
    => Parser (PinaforeScoped baseedit (Transform (PinaforeScoped baseedit) (PinaforeScoped baseedit)))
readTopLetBindings = do
    sf <- readLetBindings
    return $ do
        MkTransform f <- sf
        return $ MkTransform $ \a -> runRefNotation $ f $ liftRefNotation a

readExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (RefExpression baseedit)
readExpression = readExpressionInfixed readExpression1

readExpression1 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (RefExpression baseedit)
readExpression1 =
    (do
         spos <- getPosition
         readThis TokLambda
         args <- many readPattern
         readThis TokMap
         mval <- readExpression
         return $ do
             val <- mval
             liftRefNotation $ runSourcePos spos $ qAbstractsExpr args val) <|>
    (do
         rbmap <- readLetBindings
         readThis TokIn
         mbody <- readExpression
         return $ do
             MkTransform bmap <- liftRefNotation rbmap
             bmap mbody) <|>
    (do
         spos <- getPosition
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
             liftRefNotation $ runSourcePos spos $ qApplyAllExpr (qConstExpr qifthenelse) [etest, ethen, eelse]) <|>
    readExpression2

readExpression2 :: HasPinaforeEntityEdit baseedit => Parser (RefExpression baseedit)
readExpression2 = do
    spos <- getPosition
    te1 <- readExpression3
    targs <- many readExpression3
    return $ do
        e1 <- te1
        args <- sequence targs
        liftRefNotation $ runSourcePos spos $ qApplyAllExpr e1 args

makeEntity :: MonadFail m => PinaforeType baseedit 'PositivePolarity t -> Entity -> m t
makeEntity (ConsPinaforeType (GroundPinaforeSingularType (SimpleEntityPinaforeGroundType t) NilDolanArguments) NilPinaforeType) p =
    case t of
        TopSimpleEntityType -> return $ LeftJoinType p
        NewSimpleEntityType -> return $ LeftJoinType $ MkNewEntity p
        NamedSimpleEntityType _ -> return $ LeftJoinType $ MkNamedEntity p
        _ -> fail $ unpack $ "not an open entity type: " <> exprShow t
makeEntity t _ = fail $ unpack $ "not an open entity type: " <> exprShow t

readExpression3 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (RefExpression baseedit)
readExpression3 =
    (do
         b <- readThis TokBool
         return $ return $ qConstExpr b) <|>
    (do
         spos <- getPosition
         name <- readThis TokName
         return $ varRefExpr spos name) <|>
    (do
         n <- readThis TokNumber
         return $ return $ qConstExpr n) <|>
    (do
         str <- readThis TokString
         return $ return $ qConstExpr str) <|>
    (do
         mprop <- readProperty
         return $ liftRefNotation mprop) <|>
    (do
         spos <- getPosition
         rexpr <- readBracketed TokOpenBrace TokCloseBrace $ readExpression
         return $ refNotationQuote spos rexpr) <|>
    (do
         readThis TokUnref
         rexpr <- readExpression3
         return $ refNotationUnquote rexpr) <|>
    (do
         readThis TokEntity
         readThis TokAt
         mt <- readType3 @baseedit @('Just 'PositivePolarity)
         anchor <- readThis TokAnchor
         return $
             liftRefNotation $ do
                 SingleMPolarType (MkAnyW tp) <- mt
                 pt <- makeEntity tp $ MkEntity anchor
                 return $ qConstExprAny $ MkAnyValue tp pt) <|>
    (readParen $
     (do
          spos <- getPosition
          name <- readThis TokOperator
          return $ varRefExpr spos name) <|>
     (do
          spos <- getPosition
          ce1 <- readExpression
          mce2 <-
              optional $ do
                  readThis TokComma
                  readExpression
          case mce2 of
              Just ce2 ->
                  return $ do
                      e1 <- ce1
                      e2 <- ce2
                      liftRefNotation $
                          runSourcePos spos $
                          qApplyAllExpr (qConstExpr ((,) :: UVar "a" -> UVar "b" -> (UVar "a", UVar "b"))) [e1, e2]
              Nothing -> return ce1)) <|>
    (do
         spos <- getPosition
         mexprs <-
             readBracketed TokOpenBracket TokCloseBracket $
             (do
                  expr1 <- readExpression
                  exprs <-
                      many $ do
                          readThis TokComma
                          readExpression
                  return $ expr1 : exprs) <|>
             return []
         return $ do
             exprs <- sequence mexprs
             liftRefNotation $ runSourcePos spos $ qSequenceExpr exprs) <?>
    "expression"

readTopExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (PinaforeScoped baseedit (QExpr baseedit))
readTopExpression = do
    rexpr <- readExpression
    eof
    return $ runRefNotation rexpr

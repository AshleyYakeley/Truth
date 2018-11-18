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
                   (RefNotation baseedit (QBindList baseedit))

instance Semigroup (Declarations baseedit) where
    (MkDeclarations ta ba) <> (MkDeclarations tb bb) = MkDeclarations (ta <> tb) (liftA2 (<>) ba bb)

instance Monoid (Declarations baseedit) where
    mempty = MkDeclarations mempty $ return mempty
    mappend = (<>)

typeDeclarations :: TypeDecls baseedit -> Declarations baseedit
typeDeclarations td = MkDeclarations td $ return mempty

readBinding :: HasPinaforePointEdit baseedit => Parser (Declarations baseedit)
readBinding = do
    name <- readThis TokName
    args <- many readPattern
    readThis TokAssign
    rval <- readExpression
    return $
        MkDeclarations mempty $ do
            val <- rval
            expr <- liftRefNotation $ qAbstractsExpr args val
            return $ qBindExpr name expr

readDeclaration :: HasPinaforePointEdit baseedit => Parser (Declarations baseedit)
readDeclaration = fmap typeDeclarations readTypeDeclaration <|> readBinding

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

readLetBindings :: HasPinaforePointEdit baseedit => Parser (QExpr baseedit -> RefExpression baseedit)
readLetBindings = do
    readThis TokLet
    MkDeclarations (MkTypeDecls td) rbl <- readDeclarations
    return $ \expr ->
        td $ do
            bl <- rbl
            f <- qBindingsLetExpr bl
            liftRefNotation $ f expr

readTopLetBindings :: HasPinaforePointEdit baseedit => Parser (QExpr baseedit -> PinaforeTypeCheck (QExpr baseedit))
readTopLetBindings = do
    f <- readLetBindings
    return $ \e -> runRefNotation $ f e

readExpression ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (RefExpression baseedit)
readExpression = readExpressionInfixed readExpression1

readExpression1 ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (RefExpression baseedit)
readExpression1 =
    (do
         readThis TokLambda
         args <- many readPattern
         readThis TokMap
         mval <- readExpression
         return $ do
             val <- mval
             liftRefNotation $ qAbstractsExpr args val) <|>
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
             liftRefNotation $ qApplyAllExpr (qConstExpr qifthenelse) [etest, ethen, eelse]) <|>
    readExpression2

readExpression2 :: HasPinaforePointEdit baseedit => Parser (RefExpression baseedit)
readExpression2 = do
    te1 <- readExpression3
    targs <- many readExpression3
    return $ do
        e1 <- te1
        args <- sequence targs
        liftRefNotation $ qApplyAllExpr e1 args

makePoint :: MonadFail m => PinaforeType baseedit 'PositivePolarity t -> Point -> m t
makePoint (ConsPinaforeType (GroundPinaforeSingularType (SimpleEntityPinaforeGroundType t) NilDolanArguments) NilPinaforeType) p =
    case t of
        TopSimpleEntityType -> return $ LeftJoinType $ MkEntity p
        PointSimpleEntityType -> return $ LeftJoinType p
        NamedSimpleEntityType _ -> return $ LeftJoinType $ MkNamedEntity p
        _ -> fail $ unpack $ "not a point type: " <> exprShow t
makePoint t _ = fail $ unpack $ "not a point type: " <> exprShow t

readExpression3 ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (RefExpression baseedit)
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
    fmap liftRefNotation readProperty <|>
    (do
         rexpr <- readBracketed TokOpenBrace TokCloseBrace $ readExpression
         return $ refNotationQuote rexpr) <|>
    (do
         readThis TokUnref
         rexpr <- readExpression3
         return $ refNotationUnquote rexpr) <|>
    (do
         readThis TokPoint
         readThis TokAt
         mt <- readType3 @baseedit @('Just 'PositivePolarity)
         anchor <- readThis TokAnchor
         return $
             liftRefNotation $ do
                 SingleMPolarType (MkAnyW tp) <- mt
                 pt <- makePoint tp $ MkPoint anchor
                 return $ qConstExprAny $ MkAnyValue tp pt) <|>
    (readParen $
     (do
          name <- readThis TokOperator
          return $ return $ qVarExpr name) <|>
     (do
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
                          qApplyAllExpr (qConstExpr ((,) :: UVar "a" -> UVar "b" -> (UVar "a", UVar "b"))) [e1, e2]
              Nothing -> return ce1)) <|>
    (do
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
             liftRefNotation $ qSequenceExpr exprs) <?>
    "expression"

readTopExpression ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (PinaforeTypeCheck (QExpr baseedit))
readTopExpression = do
    rexpr <- readExpression
    return $ runRefNotation rexpr

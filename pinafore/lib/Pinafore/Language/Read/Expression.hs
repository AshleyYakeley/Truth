module Pinafore.Language.Read.Expression
    ( readTopExpression
    , readTopDeclarations
    ) where

import Pinafore.Base
import Pinafore.Language.If
import Pinafore.Language.Read.Infix
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Read.TypeDecls
import Pinafore.Language.Syntax
import Pinafore.Language.Type
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

readPattern :: Parser SyntaxPattern
readPattern = do
    name <- readThis TokName
    return $ MkSyntaxPattern name

readTypeSignature :: Parser SyntaxType
readTypeSignature = do
    readThis TokTypeJudge
    readType

readBindingRest :: HasPinaforeEntityEdit baseedit => Parser ([SyntaxPattern], SyntaxExpression baseedit)
readBindingRest = do
    args <- many readPattern
    readThis TokAssign
    rval <- readExpression
    return (args, rval)

readBinding :: HasPinaforeEntityEdit baseedit => Parser (SyntaxBinding baseedit)
readBinding = do
    spos <- getPosition
    name <- readThis TokName
    (do
         tp <- readTypeSignature
         readThis TokSemicolon
         name' <- readThis TokName
         (args, rval) <- readBindingRest
         if name == name'
             then return $ MkSyntaxBinding spos (Just tp) name' args rval
             else fail $ "type signature name " <> show name <> " does not match definition for " <> show name') <|>
        (do
             (args, rval) <- readBindingRest
             return $ MkSyntaxBinding spos Nothing name args rval)

readDeclaration :: HasPinaforeEntityEdit baseedit => Parser (SyntaxDeclarations baseedit)
readDeclaration =
    fmap typeSyntaxDeclarations readTypeDeclaration <|> fmap (MkSyntaxDeclarations mempty . pure) readBinding

readDeclarations :: HasPinaforeEntityEdit baseedit => Parser (SyntaxDeclarations baseedit)
readDeclarations =
    (do
         b <- readDeclaration
         mbl <-
             optional $ do
                 readThis TokSemicolon
                 readDeclarations
         return $ b <> fromMaybe mempty mbl) <|>
    (return mempty)

readLetBindings :: HasPinaforeEntityEdit baseedit => Parser (SyntaxDeclarations baseedit)
readLetBindings = do
    readThis TokLet
    readDeclarations

readTopDeclarations :: HasPinaforeEntityEdit baseedit => Parser (SyntaxTopDeclarations baseedit)
readTopDeclarations = do
    spos <- getPosition
    sdecls <- readLetBindings
    return $ MkSyntaxTopDeclarations spos sdecls

readSourcePos :: Parser (SyntaxExpression' baseedit) -> Parser (SyntaxExpression baseedit)
readSourcePos p = do
    spos <- getPosition
    expr' <- p
    return $ MkSyntaxExpression spos expr'

readExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (SyntaxExpression baseedit)
readExpression = readExpressionInfixed $ readSourcePos readExpression1'

readExpression1' ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (SyntaxExpression' baseedit)
readExpression1' =
    (do
         readThis TokLambda
         args <- many readPattern
         readThis TokMap
         mval <- readExpression
         return $ SEAbstract args mval) <|>
    (do
         sdecls <- readLetBindings
         readThis TokIn
         sbody <- readExpression
         return $ SELet sdecls sbody) <|>
    (do
         spos <- getPosition
         readThis TokIf
         metest <- readExpression
         readThis TokThen
         methen <- readExpression
         readThis TokElse
         meelse <- readExpression
         return $ SEApply (MkSyntaxExpression spos $ SEConst $ toValue qifthenelse) [metest, methen, meelse]) <|>
    readExpression2'

readExpression2' :: HasPinaforeEntityEdit baseedit => Parser (SyntaxExpression' baseedit)
readExpression2' = do
    sfunc <- readExpression3
    sargs <- many readExpression3
    return $ SEApply sfunc sargs

readExpression3 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (SyntaxExpression baseedit)
readExpression3 = readSourcePos readExpression3'

readExpression3' ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (SyntaxExpression' baseedit)
readExpression3' =
    (do
         b <- readThis TokBool
         return $ SEConst $ toValue b) <|>
    (do
         name <- readThis TokName
         return $ SEVar name) <|>
    (do
         n <- readThis TokNumber
         return $ SEConst $ toValue n) <|>
    (do
         str <- readThis TokString
         return $ SEConst $ toValue str) <|>
    (do
         readThis TokProperty
         readThis TokAt
         sta <- readType3
         readThis TokAt
         stb <- readType3
         anchor <- readThis TokAnchor
         return $ SEProperty sta stb anchor) <|>
    (do
         rexpr <- readBracketed TokOpenBrace TokCloseBrace $ readExpression
         return $ SERef rexpr) <|>
    (do
         readThis TokUnref
         rexpr <- readExpression3
         return $ SEUnref rexpr) <|>
    (do
         readThis TokEntity
         readThis TokAt
         mt <- readType3
         anchor <- readThis TokAnchor
         return $ SEEntity mt anchor) <|>
    (readParen $
     (do
          name <- readThis TokOperator
          return $ SEVar name) <|>
     (do
          sexpr1@(MkSyntaxExpression _ sexpr1') <- readExpression
          msexpr2 <-
              optional $ do
                  readThis TokComma
                  readExpression
          case msexpr2 of
              Just sexpr2 -> do
                  sfun <-
                      readSourcePos $ return $ SEConst $ toValue ((,) :: UVar "a" -> UVar "b" -> (UVar "a", UVar "b"))
                  return $ SEApply sfun [sexpr1, sexpr2]
              Nothing -> return sexpr1')) <|>
    (do
         sexprs <-
             readBracketed TokOpenBracket TokCloseBracket $
             (do
                  expr1 <- readExpression
                  exprs <-
                      many $ do
                          readThis TokComma
                          readExpression
                  return $ expr1 : exprs) <|>
             return []
         return $ SEList sexprs) <?>
    "expression"

readTopExpression ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (SyntaxExpression baseedit)
readTopExpression = do
    sexpr <- readExpression
    eof
    return sexpr

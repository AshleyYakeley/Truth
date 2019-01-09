module Pinafore.Language.Read.Expression
    ( readTopExpression
    , readTopDeclarations
    ) where

import Pinafore.Base
import Pinafore.Language.Read.Infix
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Read.TypeDecls
import Pinafore.Language.Syntax
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

readSourcePosPattern :: Parser SyntaxPattern' -> Parser SyntaxPattern
readSourcePosPattern p = do
    spos <- getPosition
    expr' <- p
    return $ MkSyntaxPattern spos expr'

readPatterns :: Parser [SyntaxPattern]
readPatterns = many readPattern2

readPattern1 :: Parser SyntaxPattern
readPattern1 =
    readSourcePosPattern
        (do
             consname <- readThis TokUName
             pats <- readPatterns
             return $ ConstructorSyntaxPattern consname pats) <|>
    readPattern2

readPattern2 :: Parser SyntaxPattern
readPattern2 = do
    spos <- getPosition
    pat1 <- readPattern3
    mpat2 <-
        optional $ do
            readThis TokAt
            readPattern2
    case mpat2 of
        Nothing -> return pat1
        Just pat2 -> return $ MkSyntaxPattern spos $ BothSyntaxPattern pat1 pat2

readPattern3 :: Parser SyntaxPattern
readPattern3 =
    readSourcePosPattern
        (do
             name <- readThis TokLName
             return $ VarSyntaxPattern name) <|>
    readSourcePosPattern
        (do
             readThis TokUnderscore
             return AnySyntaxPattern) <|>
    readParen readPattern1

readTypeSignature :: Parser SyntaxType
readTypeSignature = do
    readThis TokTypeJudge
    readType

readBindingRest :: HasPinaforeEntityEdit baseedit => Parser ([SyntaxPattern], SyntaxExpression baseedit)
readBindingRest = do
    args <- readPatterns
    readThis TokAssign
    rval <- readExpression
    return (args, rval)

readBinding :: HasPinaforeEntityEdit baseedit => Parser (SyntaxBinding baseedit)
readBinding = do
    spos <- getPosition
    name <- readThis TokLName
    (do
         tp <- readTypeSignature
         readThis TokSemicolon
         name' <- readThis TokLName
         (args, rval) <- readBindingRest
         if name == name'
             then return $ MkSyntaxBinding spos (Just tp) name' $ seAbstracts spos args rval
             else fail $ "type signature name " <> show name <> " does not match definition for " <> show name') <|>
        (do
             (args, rval) <- readBindingRest
             return $ MkSyntaxBinding spos Nothing name $ seAbstracts spos args rval)

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
readExpression = readExpressionInfixed readExpression1

readCase ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (SyntaxCase baseedit)
readCase = do
    pat <- readPattern1
    readThis TokMap
    e <- readExpression
    return $ MkSyntaxCase pat e

readCases ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser [SyntaxCase baseedit]
readCases =
    (do
         c <- readCase
         mcl <-
             optional $ do
                 readThis TokSemicolon
                 readCases
         return $ c : fromMaybe [] mcl) <|>
    (return [])

readExpression1 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (SyntaxExpression baseedit)
readExpression1 =
    (do
         spos <- getPosition
         readThis TokLambda
         args <- readPatterns
         readThis TokMap
         mval <- readExpression
         return $ seAbstracts spos args mval) <|>
    readSourcePos
        (do
             sdecls <- readLetBindings
             readThis TokIn
             sbody <- readExpression
             return $ SELet sdecls sbody) <|>
    readSourcePos
        (do
             readThis TokCase
             sbody <- readExpression
             readThis TokOf
             scases <- readCases
             readThis TokEnd
             return $ SECase sbody scases) <|>
    (do
         spos <- getPosition
         readThis TokIf
         metest <- readExpression
         readThis TokThen
         methen <- readExpression
         readThis TokElse
         meelse <- readExpression
         return $ seApplys spos (seConst spos SCIfThenElse) [metest, methen, meelse]) <|>
    readExpression2

readExpression2 :: HasPinaforeEntityEdit baseedit => Parser (SyntaxExpression baseedit)
readExpression2 = do
    spos <- getPosition
    sfunc <- readExpression3
    sargs <- many readExpression3
    return $ seApplys spos sfunc sargs

readExpression3 ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (SyntaxExpression baseedit)
readExpression3 =
    readSourcePos
        (do
             name <- readThis TokUName
             return $ SEConst $ SCLiteral $ SLConstructor name) <|>
    readSourcePos
        (do
             name <- readThis TokLName
             return $ SEVar name) <|>
    readSourcePos
        (do
             n <- readThis TokNumber
             return $ SEConst $ SCLiteral $ SLNumber n) <|>
    readSourcePos
        (do
             str <- readThis TokString
             return $ SEConst $ SCLiteral $ SLString str) <|>
    readSourcePos
        (do
             readThis TokProperty
             readThis TokAt
             sta <- readType3
             readThis TokAt
             stb <- readType3
             anchor <- readThis TokAnchor
             return $ SEProperty sta stb anchor) <|>
    readSourcePos
        (do
             rexpr <- readBracketed TokOpenBrace TokCloseBrace $ readExpression
             return $ SERef rexpr) <|>
    readSourcePos
        (do
             readThis TokUnref
             rexpr <- readExpression3
             return $ SEUnref rexpr) <|>
    readSourcePos
        (do
             readThis TokEntity
             readThis TokAt
             mt <- readType3
             anchor <- readThis TokAnchor
             return $ SEEntity mt anchor) <|>
    (readParen $
     readSourcePos
         (do
              name <- readThis TokOperator
              return $ SEVar name) <|>
     (do
          sexpr1 <- readExpression
          msexpr2 <-
              optional $ do
                  readThis TokComma
                  readExpression
          case msexpr2 of
              Just sexpr2 -> do
                  spos <- getPosition
                  return $ seApplys spos (seConst spos SCPair) [sexpr1, sexpr2]
              Nothing -> return sexpr1)) <|>
    readSourcePos
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

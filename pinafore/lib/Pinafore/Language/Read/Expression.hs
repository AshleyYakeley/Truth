module Pinafore.Language.Read.Expression
    ( readExpression
    , readTopDeclarations
    ) where

import Pinafore.Language.Read.Constructor
import Pinafore.Language.Read.Infix
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Pattern
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Read.TypeDecls
import Pinafore.Language.Syntax
import Shapes hiding (try)

readTypeSignature :: Parser SyntaxType
readTypeSignature = do
    readThis TokTypeJudge
    readType

readBindingRest :: Parser ([SyntaxPattern], SyntaxExpression)
readBindingRest = do
    args <- readPatterns
    readThis TokAssign
    rval <- readExpression
    return (args, rval)

readBinding :: Parser SyntaxBinding
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

readLines :: Parser a -> Parser [a]
readLines p =
    (do
         a <- p
         ma <-
             optional $ do
                 readThis TokSemicolon
                 readLines p
         return $ a : fromMaybe [] ma) <|>
    (return [])

readDeclaration :: Parser SyntaxDeclaration
readDeclaration = readTypeDeclaration <|> fmap BindingSyntaxDeclaration readBinding

readDeclarations :: Parser [SyntaxDeclaration]
readDeclarations = readLines readDeclaration

readLetBindings :: Parser [SyntaxDeclaration]
readLetBindings = do
    readThis TokLet
    readDeclarations

readTopDeclarations :: Parser SyntaxTopDeclarations
readTopDeclarations = do
    spos <- getPosition
    sdecls <- readLetBindings
    return $ MkSyntaxTopDeclarations spos sdecls

readExpression :: Parser SyntaxExpression
readExpression = readExpressionInfixed readExpression1

readCase :: Parser SyntaxCase
readCase = do
    pat <- readPattern2
    readThis TokMap
    e <- readExpression
    return $ MkSyntaxCase pat e

readCases :: Parser [SyntaxCase]
readCases = readLines readCase

data DoLine
    = ExpressionDoLine SyntaxExpression
    | BindDoLine SyntaxPattern
                 SyntaxExpression

readDoLine :: Parser DoLine
readDoLine =
    (try $ do
         pat <- readPattern2
         readThis TokBackMap
         expr <- readExpression
         return $ BindDoLine pat expr) <|>
    (do
         expr <- readExpression
         return $ ExpressionDoLine expr)

doLines ::
       forall m. MonadFail m
    => DoLine
    -> [DoLine]
    -> m SyntaxExpression
doLines (ExpressionDoLine expr) [] = return expr
doLines (BindDoLine _ _) [] = fail "last line of do block not expression"
doLines (ExpressionDoLine expra) (l:ll) = do
    exprb <- doLines l ll
    return $ seApplys (getSourcePos expra) (MkWithSourcePos (getSourcePos exprb) $ SEConst SCBind_) [expra, exprb]
doLines (BindDoLine pat expra) (l:ll) = do
    exprb <- doLines l ll
    return $
        seApplys
            (getSourcePos expra)
            (MkWithSourcePos (getSourcePos exprb) $ SEConst SCBind)
            [expra, seAbstract (getSourcePos pat) pat exprb]

readExpression1 :: Parser SyntaxExpression
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
         readThis TokDo
         dl <- readLines readDoLine
         readThis TokEnd
         case dl of
             [] -> fail "empty 'do' block"
             l:ll -> doLines l ll) <|>
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

readExpression2 :: Parser SyntaxExpression
readExpression2 = do
    spos <- getPosition
    sfunc <- readExpression3
    sargs <- many readExpression3
    return $ seApplys spos sfunc sargs

readTypeAnnotation :: Parser SyntaxType
readTypeAnnotation = do
    readThis TokAt
    readType3

readExpression3 :: Parser SyntaxExpression
readExpression3 =
    readSourcePos
        (do
             name <- readThis TokLName
             return $ SEVar name) <|>
    readSourcePos
        (do
             c <- readConstructor
             return $ SEConst $ SCConstructor c) <|>
    readSourcePos
        (do
             readThis TokProperty
             sta <- readTypeAnnotation
             stb <- readTypeAnnotation
             anchor <- readThis TokAnchor
             return $ SEConst $ SCSpecialForm $ SSFProperty sta stb anchor) <|>
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
             readThis TokOpenEntity
             mt <- readTypeAnnotation
             anchor <- readThis TokAnchor
             return $ SEConst $ SCSpecialForm $ SSFOpenEntity mt anchor) <|>
    readSourcePos
        (do
             readThis TokNewOpenEntity
             mt <- readTypeAnnotation
             return $ SEConst $ SCSpecialForm $ SSFNewOpenEntity mt) <|>
    readSourcePos
        (do
             readThis TokEvaluate
             mt <- readTypeAnnotation
             return $ SEConst $ SCSpecialForm $ SSFEvaluate mt) <|>
    (readParen $
     readSourcePos
         (do
              name <- readThis TokOperator
              return $ SEVar name) <|>
     (do
          spos <- getPosition
          msexpr1 <- optional readExpression
          case msexpr1 of
              Nothing -> return $ seConst spos $ SCConstructor SLUnit
              Just sexpr1 -> do
                  msexpr2 <-
                      optional $ do
                          readThis TokComma
                          readExpression
                  case msexpr2 of
                      Just sexpr2 -> do return $ seApplys spos (seConst spos $ SCConstructor SLPair) [sexpr1, sexpr2]
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

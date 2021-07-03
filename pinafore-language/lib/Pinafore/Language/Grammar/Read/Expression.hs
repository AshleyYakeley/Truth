module Pinafore.Language.Grammar.Read.Expression
    ( readName
    , readExpression
    , readModule
    , readTopDeclarations
    ) where

import Pinafore.Language.Grammar.Read.Constructor
import Pinafore.Language.Grammar.Read.Infix
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Pattern
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Read.TypeDecls
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
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

readModuleName :: Parser ModuleName
readModuleName =
    fmap MkModuleName $ (fmap pure $ readThis TokUName) <|> (fmap (\(nn, n) -> nn <> pure n) $ readThis TokQUName)

readImport :: Parser SyntaxDeclaration
readImport = do
    spos <- getPosition
    readThis TokImport
    mname <- readModuleName
    return $ ImportSyntaxDeclaration spos mname

readDeclaration :: Parser SyntaxDeclaration
readDeclaration = readTypeDeclaration <|> fmap BindingSyntaxDeclaration readBinding <|> readImport

readDocDeclaration :: Parser SyntaxDocDeclaration
readDocDeclaration = do
    doc <- readDocComment
    decl <- readDeclaration
    return $ MkSyntaxDocDeclaration doc decl

readDocDeclarations :: Parser [SyntaxDocDeclaration]
readDocDeclarations = readLines readDocDeclaration

readLetBindings :: Parser [SyntaxDocDeclaration]
readLetBindings = do
    readThis TokLet
    readDocDeclarations

readTopDeclarations :: Parser SyntaxTopDeclarations
readTopDeclarations = do
    spos <- getPosition
    sdecls <- readLetBindings
    return $ MkSyntaxTopDeclarations spos sdecls

readSubsumedExpression :: SyntaxExpression -> Parser SyntaxExpression
readSubsumedExpression expr = do
    mt <-
        optional $ do
            spos <- getPosition
            readThis TokTypeJudge
            t <- readType
            return (spos, t)
    case mt of
        Nothing -> return expr
        Just (spos, t) -> readSubsumedExpression $ MkWithSourcePos spos $ SESubsume expr t

readExpression :: Parser SyntaxExpression
readExpression = do
    expr <- readExpressionInfixed readExpression1
    readSubsumedExpression expr

readName :: Parser Name
readName = readThis TokUName <|> readThis TokLName <|> (readParen $ readThis TokOperator)

readModule :: Parser SyntaxModule
readModule =
    readSourcePos $
    (do
         sdecls <- readLetBindings
         readThis TokIn
         sbody <- readModule
         return $ SMLet sdecls sbody) <|>
    (do
         readThis TokExport
         names <- many readName
         return $ SMExport names)

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

readAnnotation :: Parser SyntaxAnnotation
readAnnotation =
    (do
         readThis TokAt
         t <- readType3
         return $ SAType t) <|>
    (do
         anchor <- readThis TokAnchor
         return $ SAAnchor anchor)

readExpression3 :: Parser SyntaxExpression
readExpression3 =
    readSourcePos
        (do
             name <- readReferenceLName
             annotations <- many readAnnotation
             return $
                 case annotations of
                     [] -> SEVar name
                     (a:aa) -> SESpecialForm name $ a :| aa) <|>
    readSourcePos
        (do
             c <- readConstructor
             return $ SEConst $ SCConstructor c) <|>
    readSourcePos
        (do
             rexpr <- readBracketed TokOpenBrace TokCloseBrace $ readExpression
             return $ SERef rexpr) <|>
    readSourcePos
        (do
             readThis TokUnref
             rexpr <- readExpression3
             return $ SEUnref rexpr) <|>
    (readParen $
     readSourcePos
         (do
              name <- readThis TokOperator
              return $ SEVar $ UnqualifiedReferenceName name) <|>
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

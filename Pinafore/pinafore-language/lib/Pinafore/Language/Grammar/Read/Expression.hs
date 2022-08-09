module Pinafore.Language.Grammar.Read.Expression
    ( readName
    , readExpression
    , readModule
    , readTopDeclarations
    , operatorFixity
    ) where

import Pinafore.Language.ExprShow
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

readModuleName :: Parser ModuleName
readModuleName =
    fmap MkModuleName $ (fmap pure $ readThis TokUName) <|> (fmap (\(nn, n) -> nn <> pure n) $ readThis TokQUName)

readImport :: Parser SyntaxDeclaration
readImport = do
    spos <- getPosition
    readThis TokImport
    mname <- readModuleName
    mimportnames <- optional $ readParen readNames
    return $ ImportSyntaxDeclaration spos mname mimportnames

readNames :: Parser [Name]
readNames = readCommaList readName

readExpose :: Parser SyntaxExpose
readExpose =
    (do
         sdecls <- readLetBindings
         readThis TokIn
         sbody <- readExpose
         return $ SExpLet sdecls sbody) <|>
    (do
         spos <- getPosition
         readThis TokExpose
         names <- readNames
         return $ SExpExpose spos names)

readDirectDeclaration :: Parser SyntaxRecursiveDeclaration
readDirectDeclaration = readTypeDeclaration <|> fmap BindingSyntaxDeclaration readBinding

readRecursiveDeclaration :: Parser SyntaxDeclaration
readRecursiveDeclaration = do
    spos <- getPosition
    readThis TokRec
    decls <- readLines $ readWithDoc readDirectDeclaration
    readThis TokEnd
    return $ RecursiveSyntaxDeclaration spos decls

readDeclaration :: Parser SyntaxDeclaration
readDeclaration =
    fmap DirectSyntaxDeclaration readDirectDeclaration <|> readImport <|> readRecursiveDeclaration <|> do
        spos <- getPosition
        expb <- readExpose
        return $ ExposeSyntaxDeclaration spos expb

readDocDeclarations :: Parser [SyntaxWithDoc SyntaxDeclaration]
readDocDeclarations = readLines $ readWithDoc readDeclaration

readLetBindings :: Parser [SyntaxWithDoc SyntaxDeclaration]
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

-- following Haskell
-- https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-820061
operatorFixity :: Name -> Fixity
operatorFixity "." = MkFixity AssocRight 10
operatorFixity "!." = MkFixity AssocRight 10
operatorFixity "^" = MkFixity AssocRight 9
operatorFixity "^^" = MkFixity AssocRight 9
operatorFixity "**" = MkFixity AssocRight 9
operatorFixity "!**" = MkFixity AssocLeft 9
operatorFixity "!++" = MkFixity AssocLeft 9
operatorFixity "*" = MkFixity AssocLeft 8
operatorFixity ".*" = MkFixity AssocLeft 8
operatorFixity "~*" = MkFixity AssocLeft 8
operatorFixity "/" = MkFixity AssocLeft 8
operatorFixity "~/" = MkFixity AssocLeft 8
operatorFixity "<*>" = MkFixity AssocLeft 8
operatorFixity "<:*:>" = MkFixity AssocLeft 8
operatorFixity "!$" = MkFixity AssocRight 8
operatorFixity "!$%" = MkFixity AssocRight 8
operatorFixity "!$$" = MkFixity AssocRight 8
operatorFixity "!@" = MkFixity AssocRight 8
operatorFixity "!@%" = MkFixity AssocRight 8
operatorFixity "!@@" = MkFixity AssocRight 8
operatorFixity "+" = MkFixity AssocLeft 7
operatorFixity ".+" = MkFixity AssocLeft 7
operatorFixity "~+" = MkFixity AssocLeft 7
operatorFixity "-" = MkFixity AssocLeft 7
operatorFixity ".-" = MkFixity AssocLeft 7
operatorFixity "~-" = MkFixity AssocLeft 7
operatorFixity "??" = MkFixity AssocLeft 7
operatorFixity "<+>" = MkFixity AssocLeft 7
operatorFixity "<:+:>" = MkFixity AssocLeft 7
operatorFixity "::" = MkFixity AssocRight 6
operatorFixity "++" = MkFixity AssocRight 6
operatorFixity "<>" = MkFixity AssocRight 6
operatorFixity "==" = MkFixity AssocNone 5
operatorFixity "/=" = MkFixity AssocNone 5
operatorFixity "~==" = MkFixity AssocNone 5
operatorFixity "~/=" = MkFixity AssocNone 5
operatorFixity "<=" = MkFixity AssocNone 5
operatorFixity "<" = MkFixity AssocNone 5
operatorFixity ">=" = MkFixity AssocNone 5
operatorFixity ">" = MkFixity AssocNone 5
operatorFixity "<&>" = MkFixity AssocLeft 4
operatorFixity "<:&:>" = MkFixity AssocLeft 4
operatorFixity "<:&>" = MkFixity AssocLeft 4
operatorFixity "<\\>" = MkFixity AssocLeft 4
operatorFixity "<:\\>" = MkFixity AssocLeft 4
operatorFixity "<^>" = MkFixity AssocLeft 4
operatorFixity "&&" = MkFixity AssocRight 4
operatorFixity "<|>" = MkFixity AssocLeft 3
operatorFixity "<:|:>" = MkFixity AssocLeft 3
operatorFixity "||" = MkFixity AssocRight 3
operatorFixity ":=" = MkFixity AssocNone 2
operatorFixity "+=" = MkFixity AssocNone 2
operatorFixity "-=" = MkFixity AssocNone 2
operatorFixity ">>=" = MkFixity AssocLeft 1
operatorFixity ">>" = MkFixity AssocLeft 1
operatorFixity "$" = MkFixity AssocRight 0
operatorFixity _ = MkFixity AssocLeft 10

expressionFixityReader :: FixityReader SyntaxExpression
expressionFixityReader =
    MkFixityReader
        { efrReadInfix =
              do
                  spos <- getPosition
                  name <- readThis TokOperator
                  return
                      ( name
                      , operatorFixity name
                      , \e1 e2 -> seApplys spos (MkWithSourcePos spos $ SEVar $ UnqualifiedReferenceName name) [e1, e2])
        , efrMaxPrecedence = 10
        }

readExpression :: Parser SyntaxExpression
readExpression = do
    expr <- readInfixed expressionFixityReader readExpression1
    readSubsumedExpression expr

readName :: Parser Name
readName = readThis TokUName <|> readThis TokLName <|> (readParen $ readThis TokOperator)

readModule :: Parser SyntaxModule
readModule = readExpose

readCase :: Parser SyntaxCase
readCase = do
    pat <- readPattern1
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
         pat <- readPattern1
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
         (readSourcePos $ do
              readThis TokCase
              scases <- readCases
              readThis TokEnd
              return $ SELambdaCase scases) <|>
             (do
                  args <- readPatterns
                  readThis TokMap
                  mval <- readExpression
                  return $ seAbstracts spos args mval)) <|>
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
                  lsexpr2 <-
                      many $ do
                          readThis TokComma
                          readExpression
                  return $
                      case lsexpr2 of
                          [] -> sexpr1
                          sexpr2:sexprs -> let
                              appair :: SyntaxExpression -> SyntaxExpression -> SyntaxExpression
                              appair e1 e2 = seApplys spos (seConst spos $ SCConstructor SLPair) [e1, e2]
                              aptuple :: SyntaxExpression -> SyntaxExpression -> [SyntaxExpression] -> SyntaxExpression
                              aptuple e1 e2 [] = appair e1 e2
                              aptuple e1 e2 (e3:er) = appair e1 $ aptuple e2 e3 er
                              in aptuple sexpr1 sexpr2 sexprs)) <|>
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

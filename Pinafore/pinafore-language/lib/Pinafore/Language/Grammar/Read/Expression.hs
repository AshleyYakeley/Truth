module Pinafore.Language.Grammar.Read.Expression
    ( readName
    , readExpression
    , readModule
    , readTopDeclarations
    , operatorFixity
    ) where

import Language.Expression.Dolan
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Grammar.Read.Constructor
import Pinafore.Language.Grammar.Read.Infix
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Pattern
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Shapes hiding (try)

readOpenEntityTypeDeclaration :: Parser SyntaxRecursiveDeclaration'
readOpenEntityTypeDeclaration = do
    readThis TokOpenType
    name <- readTypeNewName
    return $ TypeSyntaxDeclaration name OpenEntitySyntaxTypeDeclaration

readSubtypeDeclaration :: Parser SyntaxRecursiveDeclaration'
readSubtypeDeclaration = do
    readThis TokSubtype
    trustme <-
        fmap
            (\case
                 Just () -> TrustMe
                 Nothing -> Verify) $
        optional $ readThis TokTrustMe
    sta <- readType
    readThis TokSubtypeOf
    stb <- readType
    mbody <-
        optional $ do
            readThis TokAssign
            readExpression
    return $ SubtypeSyntaxDeclaration trustme sta stb mbody

readDataTypeConstructor :: Parser SyntaxDatatypeConstructorOrSubtype
readDataTypeConstructor =
    (do
         readThis TokSubtype
         readThis TokDataType
         name <- readTypeNewName
         readThis TokOf
         constructors <- readLines $ readWithDoc readDataTypeConstructor
         readThis TokEnd
         return $ SubtypeSyntaxConstructorOrSubtype name constructors) <|>
    (do
         consName <- readThis TokUName
         mtypes <- many readType3
         return $ ConstructorSyntaxConstructorOrSubtype consName mtypes ())

readClosedTypeConstructor :: Parser SyntaxClosedEntityConstructorOrSubtype
readClosedTypeConstructor =
    (do
         readThis TokSubtype
         readThis TokClosedType
         name <- readTypeNewName
         readThis TokOf
         constructors <- readLines $ readWithDoc readClosedTypeConstructor
         readThis TokEnd
         return $ SubtypeSyntaxConstructorOrSubtype name constructors) <|>
    (do
         consName <- readThis TokUName
         mtypes <- many readType3
         anchor <- readThis TokAnchor
         return $ ConstructorSyntaxConstructorOrSubtype consName mtypes anchor)

readPositiveParameter :: Parser Name
readPositiveParameter = do
    readExactlyThis TokOperator "+"
    readTypeVar

readNegativeParameter :: Parser Name
readNegativeParameter = do
    readExactlyThis TokOperator "-"
    readTypeVar

readTypeParameter :: Parser SyntaxTypeParameter
readTypeParameter =
    fmap PositiveSyntaxTypeParameter readPositiveParameter <|> fmap NegativeSyntaxTypeParameter readNegativeParameter <|>
    (readBracketed TokOpenBrace TokCloseBrace $
     (do
          varp <- readNegativeParameter
          readThis TokComma
          varq <- readPositiveParameter
          return $ RangeSyntaxTypeParameter varp varq) <|>
     (do
          varq <- readPositiveParameter
          readThis TokComma
          varp <- readNegativeParameter
          return $ RangeSyntaxTypeParameter varp varq))

readDataTypeDeclaration :: Parser SyntaxRecursiveDeclaration'
readDataTypeDeclaration = do
    readThis TokDataType
    name <- readTypeNewName
    parameters <- many readTypeParameter
    readThis TokOf
    constructors <- readLines $ readWithDoc readDataTypeConstructor
    readThis TokEnd
    return $ TypeSyntaxDeclaration  name $ DatatypeSyntaxTypeDeclaration parameters constructors

readClosedTypeDeclaration :: Parser SyntaxRecursiveDeclaration'
readClosedTypeDeclaration = do
    readThis TokClosedType
    name <- readTypeNewName
    parameters <- many readTypeParameter
    readThis TokOf
    constructors <- readLines $ readWithDoc readClosedTypeConstructor
    readThis TokEnd
    return $ TypeSyntaxDeclaration  name $ ClosedEntitySyntaxTypeDeclaration parameters constructors

readDynamicTypeConstructor :: Parser SyntaxDynamicEntityConstructor
readDynamicTypeConstructor =
    fmap AnchorSyntaxDynamicEntityConstructor (readThis TokAnchor) <|>
    fmap NameSyntaxDynamicEntityConstructor readTypeReferenceName

readDynamicTypeDeclaration :: Parser SyntaxRecursiveDeclaration'
readDynamicTypeDeclaration = do
    readThis TokDynamicType
    name <- readTypeNewName
    readThis TokAssign
    tcons <- readSeparated1 (readThis TokOr) $ fmap pure readDynamicTypeConstructor
    return $ TypeSyntaxDeclaration  name $ DynamicEntitySyntaxTypeDeclaration tcons

readTypeDeclaration :: Parser SyntaxRecursiveDeclaration'
readTypeDeclaration =
    readOpenEntityTypeDeclaration <|> readSubtypeDeclaration <|> readDataTypeDeclaration <|> readClosedTypeDeclaration <|>
    readDynamicTypeDeclaration

readBinding :: Parser SyntaxBinding
readBinding = do
    name <- readThis TokLName
    mtp <-
        optional $ do
            readThis TokTypeJudge
            readType
    readThis TokAssign
    defn <- readExpression
    return $ MkSyntaxBinding  mtp name defn

readModuleName :: Parser ModuleName
readModuleName =
    fmap MkModuleName $ (fmap pure $ readThis TokUName) <|> (fmap (\(nn, n) -> nn <> pure n) $ readThis TokQUName)

readImport :: Parser SyntaxDeclaration'
readImport = do
    readThis TokImport
    mname <- readModuleName
    mimportnames <- optional $ readParen readNames
    return $ ImportSyntaxDeclaration  mname mimportnames

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

readDirectDeclaration :: Parser SyntaxRecursiveDeclaration'
readDirectDeclaration = readTypeDeclaration <|> fmap BindingSyntaxDeclaration readBinding

readRecursiveDeclaration :: Parser SyntaxDeclaration'
readRecursiveDeclaration = do
    readThis TokRec
    decls <- readLines $ readWithDoc $ readSourcePos readDirectDeclaration
    readThis TokEnd
    return $ RecursiveSyntaxDeclaration  decls

readDeclaration :: Parser SyntaxDeclaration
readDeclaration = readWithDoc $ readSourcePos $
    fmap DirectSyntaxDeclaration readDirectDeclaration <|> readImport <|> readRecursiveDeclaration <|> do
        expb <- readExpose
        return $ ExposeSyntaxDeclaration  expb

readDocDeclarations :: Parser [ SyntaxDeclaration]
readDocDeclarations = readLines readDeclaration

readLetBindings :: Parser [SyntaxDeclaration]
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
operatorFixity ">-" = MkFixity AssocRight 1
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

readMatch :: Parser SyntaxMatch
readMatch = do
    pat <- readPattern1
    readThis TokMap
    expr <- readExpression
    return $ MkSyntaxMatch pat expr

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

readMultimatch :: Parser (Some SyntaxMultimatch)
readMultimatch = do
    patlist <- readPatterns
    readThis TokMap
    expr <- readExpression
    return $ fixedFromList patlist $ \_ pats -> MkSome $ MkSyntaxMultimatch pats expr

getMultimatch :: PeanoNatType n -> Some SyntaxMultimatch -> Parser (SyntaxMultimatch n)
getMultimatch expected (MkSome mm) = let
    found = syntaxMultimatchLength mm
    in case testEquality expected found of
           Just Refl -> return mm
           Nothing ->
               throw $
               MatchesDifferentCount (peanoToNatural $ witnessToValue expected) (peanoToNatural $ witnessToValue found)

readExpression1 :: Parser SyntaxExpression
readExpression1 =
    (do
         spos <- getPosition
         readThis TokFn
         match <- readMatch
         return $ MkWithSourcePos spos $ SEAbstract match) <|>
    (do
         spos <- getPosition
         readThis TokFns
         mmatch <- readMultimatch
         return $ MkWithSourcePos spos $ SEAbstracts mmatch) <|>
    readSourcePos
        (do
             readThis TokMatch
             smatches <- readLines readMatch
             readThis TokEnd
             return $ SEMatch smatches) <|>
    readSourcePos
        (do
             readThis TokMatches
             smultimatches <- readLines1 readMultimatch
             readThis TokEnd
             case head smultimatches of
                 MkSome m -> do
                     let n = syntaxMultimatchLength m
                     smm <- for (toList smultimatches) $ getMultimatch n
                     return $ SEMatches $ MkSyntaxMultimatchList n smm) <|>
    readSourcePos
        (do
             sdecls <- readLetBindings
             readThis TokIn
             sbody <- readExpression
             return $ SELet sdecls sbody) <|>
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
             readThis TokUnquote
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

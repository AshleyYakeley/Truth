module Pinafore.Language.Grammar.Read.Expression
    ( readExpression
    , readModule
    , operatorFixity
    , DoLine(..)
    , readDoLine
    ) where

import Language.Expression.Dolan
import Pinafore.Language.Error
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

readSignature :: Parser SyntaxSignature
readSignature =
    readWithDoc $
    readWithSourcePos $
    (do
         name <- readLName
         readThis TokTypeJudge
         t <- readType
         mdefv <-
             optional $ do
                 readThis TokAssign
                 readExpression
         return $ ValueSyntaxSignature name t mdefv) <|>
    (do
         name <- readFullUName
         return $ SupertypeConstructorSyntaxSignature name)

readPlainDataTypeConstructor :: Parser SyntaxPlainDatatypeConstructorOrSubtype
readPlainDataTypeConstructor =
    (do
         readThis TokSubtype
         readThis TokDataType
         name <- readTypeNewName
         constructors <- readWithNamespaceName (fnName name) $ readOf $ readWithDoc readPlainDataTypeConstructor
         return $ SubtypeSyntaxConstructorOrSubtype name constructors) <|>
    (do
         consName <- readNewUName
         fmap (ConstructorSyntaxConstructorOrSubtype consName) $
             (do
                  sigs <- readOf readSignature
                  return $ RecordSyntaxConstructor sigs) <|>
             (do
                  mtypes <- many readType3
                  return $ PlainSyntaxConstructor mtypes ()))

readStorableDataTypeConstructor :: Parser SyntaxStorableDatatypeConstructorOrSubtype
readStorableDataTypeConstructor =
    (do
         readThis TokSubtype
         readThis TokDataType
         readThis TokStorable
         name <- readTypeNewName
         constructors <- readWithNamespaceName (fnName name) $ readOf $ readWithDoc readStorableDataTypeConstructor
         return $ SubtypeSyntaxConstructorOrSubtype name constructors) <|>
    (do
         consName <- readNewUName
         mtypes <- many readType3
         anchor <- readThis TokAnchor
         return $ ConstructorSyntaxConstructorOrSubtype consName $ PlainSyntaxConstructor mtypes anchor)

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
    storable <- optional $ readThis TokStorable
    name <- readTypeNewName
    parameters <- many readTypeParameter
    msupertype <-
        optional $ do
            readThis TokSubtypeOf
            readType
    readWithNamespaceName (fnName name) $
        case storable of
            Just () -> do
                case msupertype of
                    Nothing -> return ()
                    Just _ -> throw $ DeclareDatatypeStorableSupertypeError name
                constructors <- readOf $ readWithDoc readStorableDataTypeConstructor
                return $ TypeSyntaxDeclaration name $ StorableDatatypeSyntaxTypeDeclaration parameters constructors
            Nothing -> do
                constructors <- readOf $ readWithDoc readPlainDataTypeConstructor
                return $
                    TypeSyntaxDeclaration name $ PlainDatatypeSyntaxTypeDeclaration parameters msupertype constructors

readDynamicTypeBody :: Parser SyntaxTypeDeclaration
readDynamicTypeBody =
    (do
         readThis TokAssign
         anchor <- readThis TokAnchor
         return $ ConcreteDynamicEntitySyntaxTypeDeclaration anchor) <|>
    (return AbstractDynamicEntitySyntaxTypeDeclaration)

readDynamicTypeDeclaration :: Parser SyntaxRecursiveDeclaration'
readDynamicTypeDeclaration = do
    readThis TokDynamicType
    name <- readTypeNewName
    body <- readDynamicTypeBody
    return $ TypeSyntaxDeclaration name body

readTypeDeclaration :: Parser SyntaxRecursiveDeclaration'
readTypeDeclaration =
    readOpenEntityTypeDeclaration <|> readSubtypeDeclaration <|> readDataTypeDeclaration <|> readDynamicTypeDeclaration

readBinding :: Parser SyntaxBinding
readBinding = do
    pat <- readPattern
    readThis TokAssign
    defn <- readExpression
    return $ MkSyntaxBinding pat defn

readNamespaceWith :: Parser SyntaxNamespaceWith
readNamespaceWith = do
    ns <- readNamespace
    mnritems <-
        optional $ do
            neg <- optional $ readThis TokExcept
            ritems <- readParen $ readCommaList readNameRefItem
            return (not $ isJust neg, ritems)
    masns <-
        optional $ do
            readThis TokAs
            readNamespace
    curns <- readAskNamespace
    return $ MkSyntaxNamespaceWith ns mnritems (fromMaybe curns masns)

readNamespaceDecl :: Parser SyntaxDeclaration'
readNamespaceDecl = do
    readThis TokNamespace
    curns <- readAskNamespace
    name <- readUName
    let ns = namespaceAppend [name] curns
    decls <- readWithNamespace ns $ readOf readDeclaration
    return $ NamespaceSyntaxDeclaration ns decls

readNameRefItem :: Parser SyntaxNameRefItem
readNameRefItem =
    (do
         readThis TokNamespace
         name <- readNamespaceRef
         return $ NamespaceSyntaxNameRefItem name) <|>
    fmap NameSyntaxNameRefItem readFullNameRef

readDirectDeclaration :: Parser SyntaxRecursiveDeclaration'
readDirectDeclaration = readTypeDeclaration <|> fmap BindingSyntaxDeclaration readBinding

readDeclaratorDeclaration :: Parser SyntaxDeclaration'
readDeclaratorDeclaration = do
    sdeclarator <- readDeclarator
    (do
         readThis TokEnd
         return $ DeclaratorSyntaxDeclaration sdeclarator) <|>
        (do
             readThis TokIn
             sdecl <- readDeclaration
             return $ DeclaratorInSyntaxDeclaration sdeclarator sdecl)

readExposeDeclaration :: Parser SyntaxDeclaration'
readExposeDeclaration = do
    readThis TokExpose
    items <- readCommaList readNameRefItem
    return $ ExposeDeclaration items

readDebugDeclaration :: Parser SyntaxDeclaration'
readDebugDeclaration = do
    readThis TokDebug
    name <- readFullNameRef
    return $ DebugSyntaxDeclaration name

readDeclaration :: Parser SyntaxDeclaration
readDeclaration =
    readWithDoc $
    readWithSourcePos $
    choice
        [ readDebugDeclaration
        , readExposeDeclaration
        , readDeclaratorDeclaration
        , fmap DirectSyntaxDeclaration readDirectDeclaration
        , readNamespaceDecl
        ]

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

readMkVar :: FullNameRef -> Parser SyntaxExpression'
readMkVar nref = do
    curns <- readAskNamespace
    return $ SEVar curns nref

-- following Haskell
-- https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-820061
operatorFixity :: Name -> Fixity
operatorFixity "." = MkFixity AssocRight 10
operatorFixity "^" = MkFixity AssocRight 9
operatorFixity "**" = MkFixity AssocLeft 9
operatorFixity "++" = MkFixity AssocLeft 9
operatorFixity "*" = MkFixity AssocLeft 8
operatorFixity "/" = MkFixity AssocLeft 8
operatorFixity "<*>" = MkFixity AssocLeft 8
operatorFixity "<:*:>" = MkFixity AssocLeft 8
operatorFixity "!$" = MkFixity AssocRight 8
operatorFixity "!$%" = MkFixity AssocRight 8
operatorFixity "!$$" = MkFixity AssocRight 8
operatorFixity "!@" = MkFixity AssocRight 8
operatorFixity "!@%" = MkFixity AssocRight 8
operatorFixity "!@@" = MkFixity AssocRight 8
operatorFixity "+" = MkFixity AssocLeft 7
operatorFixity "-" = MkFixity AssocLeft 7
operatorFixity "??" = MkFixity AssocLeft 7
operatorFixity "<+>" = MkFixity AssocLeft 7
operatorFixity "<:+:>" = MkFixity AssocLeft 7
operatorFixity "::" = MkFixity AssocRight 6
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
                  tnames <- readThis TokOperator
                  let name = tokenNamesToFullNameRef tnames
                  var <- readMkVar name
                  return
                      ( name
                      , operatorFixity $ tnName tnames
                      , \e1 e2 -> seApplys spos (MkWithSourcePos spos var) [e1, e2])
        , efrMaxPrecedence = 10
        }

readExpression :: Parser SyntaxExpression
readExpression = do
    expr <- readInfixed expressionFixityReader readExpression1
    readSubsumedExpression expr

readModule :: Parser SyntaxModule
readModule = do
    sdecls <- readLines readDeclaration
    return $ MkSyntaxModule sdecls

data DoLine
    = ExpressionDoLine SyntaxExpression
    | DeclarationDoLine SyntaxDeclaration
    | BindDoLine SyntaxPattern
                 SyntaxExpression

readDoLine :: Parser DoLine
readDoLine =
    (try $ do
         pat <- readPattern
         readThis TokBackMap
         expr <- readExpression
         return $ BindDoLine pat expr) <|>
    (try $ do
         decl <- readDeclaration
         return $ DeclarationDoLine decl) <|>
    (do
         expr <- readExpression
         return $ ExpressionDoLine expr)

doLines :: DoLine -> [DoLine] -> Parser SyntaxExpression
doLines (ExpressionDoLine expr) [] = return expr
doLines _ [] = fail "last line of do block not expression"
doLines (ExpressionDoLine expra) (l:ll) = do
    exprb <- doLines l ll
    var <- readMkVar $ UnqualifiedFullNameRef ">>"
    return $ seApplys (getSourcePos expra) (MkWithSourcePos (getSourcePos exprb) var) [expra, exprb]
doLines (BindDoLine pat expra) (l:ll) = do
    exprb <- doLines l ll
    var <- readMkVar $ UnqualifiedFullNameRef ">>="
    return $
        seApplys
            (getSourcePos expra)
            (MkWithSourcePos (getSourcePos exprb) var)
            [expra, seAbstract (getSourcePos pat) pat exprb]
doLines (DeclarationDoLine sdecl@(MkSyntaxWithDoc _ (MkWithSourcePos spos _))) (l:ll) = do
    exprb <- doLines l ll
    return $ MkWithSourcePos spos $ SEDecl (SDLetSeq [sdecl]) exprb

readMulticase :: Parser (Some SyntaxMulticase)
readMulticase = do
    patlist <- readCommaList readPattern
    readThis TokMap
    expr <- readExpression
    return $ fixedFromList patlist $ \_ pats -> MkSome $ MkSyntaxMulticase pats expr

getMulticase :: PeanoNatType n -> Some SyntaxMulticase -> Parser (SyntaxMulticase n)
getMulticase expected (MkSome mm) = let
    found = syntaxMulticaseLength mm
    in case testEquality expected found of
           Just Refl -> return mm
           Nothing ->
               throw $
               MatchesDifferentCount (peanoToNatural $ witnessToValue expected) (peanoToNatural $ witnessToValue found)

withWithExpr :: Namespace -> [FullNameRef] -> Parser SyntaxExpression -> Parser SyntaxExpression'
withWithExpr ns names mexpr = do
    curns <- readAskNamespace
    expr <- mexpr
    return $ SEDecl (SDWith [MkSyntaxNamespaceWith ns (Just (True, fmap NameSyntaxNameRefItem names)) curns]) expr

readDeclarator :: Parser SyntaxDeclarator
readDeclarator =
    (do
         readThis TokLet
         (do
              readThis TokRec
              sdecls <- readLines $ readWithDoc $ readWithSourcePos readDirectDeclaration
              return $ SDLetRec sdecls) <|>
             (do
                  sdecls <- readLines readDeclaration
                  return $ SDLetSeq sdecls)) <|>
    (do
         readThis TokWith
         snws <- readCommaList readNamespaceWith
         return $ SDWith snws) <|>
    (do
         readThis TokImport
         mqual <- optional readLName
         simps <- readCommaList $ readThis TokString
         return $ SDImport mqual simps)

readExpression1 :: Parser SyntaxExpression
readExpression1 =
    readWithSourcePos
        (do
             readThis TokFn
             mmatch <- readMulticase
             return $ SEAbstracts mmatch) <|>
    readWithSourcePos
        (do
             readThis TokMatch
             multimatches <- readLines readMulticase
             readThis TokEnd
             case multimatches of
                 [] -> return $ SEMatch []
                 MkSome m:_ -> do
                     let n = syntaxMulticaseLength m
                     smm <- for multimatches $ getMulticase n
                     return $ SEMatches $ MkSyntaxMulticaseList n smm) <|>
    readWithSourcePos
        (do
             sdecl <- readDeclarator
             readThis TokIn
             sbody <- readExpression
             return $ SEDecl sdecl sbody) <|>
    readWithSourcePos
        (do
             readThis TokDo
             mns <- optional readNamespaceQualifier
             let ns = fromMaybe "Action." mns
             withWithExpr ns ["map", "pure", "ap", "liftA2", "**", ">>", ">>="] $ do
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
    readWithSourcePos
        (do
             readThis TokDebug
             text <- readThis TokString
             expr <- readExpression1
             return $ SEDebug text expr) <|>
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
    readWithSourcePos
        (do
             name <- readFullLName
             annotations <- many readAnnotation
             case annotations of
                 [] -> readMkVar name
                 (a:aa) -> return $ SESpecialForm name $ a :| aa) <|>
    readWithSourcePos
        (do
             c <- readConstructor $ Just readExpression
             return $ SEConst $ SCConstructor c) <|>
    readWithSourcePos
        (readBracketed TokOpenBrace TokCloseBrace $ do
             mns <- optional readNamespaceQualifier
             let ns = fromMaybe "WholeModel." mns
             withWithExpr ns ["map", "pure", "ap", "liftA2", "**", ">>"] $
                 readWithSourcePos $ do
                     rexpr <- readExpression
                     return $ SEAppQuote rexpr) <|>
    readWithSourcePos
        (do
             readThis TokUnquote
             rexpr <- readExpression3
             return $ SEAppUnquote rexpr) <|>
    (readParen $
     readWithSourcePos
         (do
              tnames <- readThis TokOperator
              readMkVar $ tokenNamesToFullNameRef tnames) <|>
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
    readWithSourcePos
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

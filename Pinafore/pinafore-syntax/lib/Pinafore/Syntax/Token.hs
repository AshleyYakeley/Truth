module Pinafore.Syntax.Token
    ( TokenNames (..)
    , tokenNamesToFullNameRef
    , tokenNamesToSingleName
    , tokenNamesToNamespaceRef
    , Comment (..)
    , Token (..)
    , fixedTokenName
    )
where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name
import Pinafore.Syntax.ReadShow

data Comment
    = BlockComment String
    | LineComment String

instance Show Comment where
    show (BlockComment s) = "{#" <> s <> "#}"
    show (LineComment s) = "#" <> s

instance ShowText Comment where
    showText = pack . show

data TokenNames = MkTokenNames
    { tnAbsolute :: Bool
    , tnName :: Name
    , tnSpace :: [Name] -- always upper
    }
    deriving stock Eq

instance ShowText TokenNames where
    showText MkTokenNames{..} =
        showText tnName
            <> concatmap (\n -> "." <> showText n) tnSpace
            <> if tnAbsolute
                then "."
                else ""

instance IsString TokenNames where
    fromString s = let
        tnAbsolute = False
        tnName = fromString s
        tnSpace = []
        in MkTokenNames{..}

tokenNamesToFullNameRef :: TokenNames -> FullNameRef
tokenNamesToFullNameRef MkTokenNames{..} =
    MkFullNameRef tnName
        $ ( if tnAbsolute
                then AbsoluteNamespaceRef . MkNamespace
                else RelativeNamespaceRef
          )
            tnSpace

tokenNamesToSingleName :: TokenNames -> Maybe Name
tokenNamesToSingleName MkTokenNames{..} = do
    guard $ not tnAbsolute
    guard $ null tnSpace
    return tnName

tokenNamesToNamespaceRef :: TokenNames -> NamespaceRef
tokenNamesToNamespaceRef MkTokenNames{..} =
    ( if tnAbsolute
        then AbsoluteNamespaceRef . MkNamespace
        else RelativeNamespaceRef
    )
        $ [tnName]
        <> tnSpace

showTokenContents :: Token a -> Maybe (a -> Text)
showTokenContents TokComment = Just showText
showTokenContents TokNamesUpper = Just showText
showTokenContents TokNamesLower = Just showText
showTokenContents TokAnchor = Just showText
showTokenContents TokOperator = Just showText
showTokenContents TokNumber = Just $ toText . showPinafore
showTokenContents _ = Nothing

data Token t where
    TokComment :: Token Comment
    TokSemicolon :: Token ()
    TokComma :: Token ()
    TokTypeJudge :: Token ()
    TokTypeDynamic :: Token ()
    TokOpenParen :: Token ()
    TokCloseParen :: Token ()
    TokOpenBracket :: Token ()
    TokCloseBracket :: Token ()
    TokOpenBrace :: Token ()
    TokCloseBrace :: Token ()
    TokSpliceOpenBrace :: Token ()
    TokString :: Token Text
    TokUnquote :: Token ()
    TokRec :: Token ()
    TokLet :: Token ()
    TokImply :: Token ()
    TokAp :: Token ()
    TokDo :: Token ()
    TokIf :: Token ()
    TokThen :: Token ()
    TokElse :: Token ()
    TokType :: Token ()
    TokDataType :: Token ()
    TokEntityType :: Token ()
    TokPredicateType :: Token ()
    TokSubtype :: Token ()
    TokTrustMe :: Token ()
    TokStorable :: Token ()
    TokExpose :: Token ()
    TokImport :: Token ()
    TokAs :: Token ()
    TokExcept :: Token ()
    TokNamespace :: Token ()
    TokDocSec :: Token ()
    TokWith :: Token ()
    TokNamesUpper :: Token TokenNames
    TokNamesLower :: Token TokenNames
    TokImplicitName :: Token ImplicitName
    TokUnderscore :: Token ()
    TokFn :: Token ()
    TokAssign :: Token ()
    TokMap :: Token ()
    TokBackMap :: Token ()
    TokAnchor :: Token Anchor
    TokSpecialName :: Token Name
    TokAt :: Token ()
    TokOperator :: Token TokenNames
    TokSubtypeOf :: Token ()
    TokOr :: Token ()
    TokAnd :: Token ()
    TokNumber :: Token Number
    TokDebug :: Token ()

instance TestEquality Token where
    testEquality TokComment TokComment = Just Refl
    testEquality TokSemicolon TokSemicolon = Just Refl
    testEquality TokComma TokComma = Just Refl
    testEquality TokTypeJudge TokTypeJudge = Just Refl
    testEquality TokTypeDynamic TokTypeDynamic = Just Refl
    testEquality TokOpenParen TokOpenParen = Just Refl
    testEquality TokCloseParen TokCloseParen = Just Refl
    testEquality TokOpenBracket TokOpenBracket = Just Refl
    testEquality TokCloseBracket TokCloseBracket = Just Refl
    testEquality TokSpliceOpenBrace TokSpliceOpenBrace = Just Refl
    testEquality TokOpenBrace TokOpenBrace = Just Refl
    testEquality TokCloseBrace TokCloseBrace = Just Refl
    testEquality TokString TokString = Just Refl
    testEquality TokUnquote TokUnquote = Just Refl
    testEquality TokRec TokRec = Just Refl
    testEquality TokLet TokLet = Just Refl
    testEquality TokImply TokImply = Just Refl
    testEquality TokAp TokAp = Just Refl
    testEquality TokDo TokDo = Just Refl
    testEquality TokIf TokIf = Just Refl
    testEquality TokThen TokThen = Just Refl
    testEquality TokElse TokElse = Just Refl
    testEquality TokType TokType = Just Refl
    testEquality TokDataType TokDataType = Just Refl
    testEquality TokEntityType TokEntityType = Just Refl
    testEquality TokPredicateType TokPredicateType = Just Refl
    testEquality TokSubtype TokSubtype = Just Refl
    testEquality TokTrustMe TokTrustMe = Just Refl
    testEquality TokStorable TokStorable = Just Refl
    testEquality TokExpose TokExpose = Just Refl
    testEquality TokImport TokImport = Just Refl
    testEquality TokAs TokAs = Just Refl
    testEquality TokExcept TokExcept = Just Refl
    testEquality TokNamespace TokNamespace = Just Refl
    testEquality TokDocSec TokDocSec = Just Refl
    testEquality TokWith TokWith = Just Refl
    testEquality TokNamesUpper TokNamesUpper = Just Refl
    testEquality TokNamesLower TokNamesLower = Just Refl
    testEquality TokImplicitName TokImplicitName = Just Refl
    testEquality TokUnderscore TokUnderscore = Just Refl
    testEquality TokFn TokFn = Just Refl
    testEquality TokAssign TokAssign = Just Refl
    testEquality TokMap TokMap = Just Refl
    testEquality TokBackMap TokBackMap = Just Refl
    testEquality TokAnchor TokAnchor = Just Refl
    testEquality TokSpecialName TokSpecialName = Just Refl
    testEquality TokAt TokAt = Just Refl
    testEquality TokOperator TokOperator = Just Refl
    testEquality TokSubtypeOf TokSubtypeOf = Just Refl
    testEquality TokOr TokOr = Just Refl
    testEquality TokAnd TokAnd = Just Refl
    testEquality TokNumber TokNumber = Just Refl
    testEquality TokDebug TokDebug = Just Refl
    testEquality _ _ = Nothing

data TokenName t where
    FixedTokenName :: Text -> TokenName ()
    VarTokenName :: ((t :~: ()) -> Void) -> Text -> TokenName t

tokenName :: Token t -> TokenName t
tokenName TokComment = VarTokenName (\case {}) "comment"
tokenName TokSemicolon = FixedTokenName ";"
tokenName TokComma = FixedTokenName ","
tokenName TokTypeJudge = FixedTokenName ":"
tokenName TokTypeDynamic = FixedTokenName ":?"
tokenName TokOpenParen = FixedTokenName "("
tokenName TokCloseParen = FixedTokenName ")"
tokenName TokOpenBracket = FixedTokenName "["
tokenName TokCloseBracket = FixedTokenName "]"
tokenName TokSpliceOpenBrace = FixedTokenName "!{"
tokenName TokOpenBrace = FixedTokenName "{"
tokenName TokCloseBrace = FixedTokenName "}"
tokenName TokString = VarTokenName (\case {}) "quoted string"
tokenName TokUnquote = FixedTokenName "%"
tokenName TokRec = FixedTokenName "rec"
tokenName TokLet = FixedTokenName "let"
tokenName TokImply = FixedTokenName "imply"
tokenName TokAp = FixedTokenName "ap"
tokenName TokDo = FixedTokenName "do"
tokenName TokIf = FixedTokenName "if"
tokenName TokThen = FixedTokenName "then"
tokenName TokElse = FixedTokenName "else"
tokenName TokType = FixedTokenName "type"
tokenName TokDataType = FixedTokenName "datatype"
tokenName TokEntityType = FixedTokenName "entitytype"
tokenName TokPredicateType = FixedTokenName "predicatetype"
tokenName TokSubtype = FixedTokenName "subtype"
tokenName TokTrustMe = FixedTokenName "trustme"
tokenName TokStorable = FixedTokenName "storable"
tokenName TokExpose = FixedTokenName "expose"
tokenName TokImport = FixedTokenName "import"
tokenName TokAs = FixedTokenName "as"
tokenName TokExcept = FixedTokenName "except"
tokenName TokNamespace = FixedTokenName "namespace"
tokenName TokDocSec = FixedTokenName "docsec"
tokenName TokWith = FixedTokenName "with"
tokenName TokNamesUpper = VarTokenName (\case {}) "unames"
tokenName TokNamesLower = VarTokenName (\case {}) "lnames"
tokenName TokImplicitName = VarTokenName (\case {}) "implicit name"
tokenName TokUnderscore = FixedTokenName "_"
tokenName TokFn = FixedTokenName "fn"
tokenName TokAssign = FixedTokenName "="
tokenName TokMap = FixedTokenName "=>"
tokenName TokBackMap = FixedTokenName "<-"
tokenName TokAnchor = VarTokenName (\case {}) "anchor"
tokenName TokSpecialName = VarTokenName (\case {}) "special name"
tokenName TokAt = FixedTokenName "@"
tokenName TokOperator = VarTokenName (\case {}) "infix"
tokenName TokSubtypeOf = FixedTokenName "<:"
tokenName TokOr = FixedTokenName "|"
tokenName TokAnd = FixedTokenName "&"
tokenName TokNumber = VarTokenName (\case {}) "number"
tokenName TokDebug = FixedTokenName "debug"

fixedTokenName :: Token () -> Text
fixedTokenName tok =
    case tokenName tok of
        FixedTokenName t -> t
        VarTokenName v _ -> absurd $ v Refl

instance ShowText (Token t) where
    showText tok =
        case tokenName tok of
            FixedTokenName t -> toText $ showPinafore t
            VarTokenName _ s -> s

instance Show (Token t) where
    show = unpack . showText

instance ShowText (SomeOf Token) where
    showText (MkSomeOf t x) =
        showText t
            <> case showTokenContents t of
                Just f -> "(" <> f x <> ")"
                Nothing -> ""

instance Show (SomeOf Token) where
    show = unpack . showText

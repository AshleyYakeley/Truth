module Pinafore.Syntax.Doc.SyntaxDoc
    ( constructorDocItem
    , typeDocItem
    , typeDeclDoc
    , valueDocItem
    ) where

import Pinafore.Syntax.Doc.DefDoc
import Pinafore.Syntax.Doc.Docs
import Pinafore.Syntax.Name
import Pinafore.Syntax.Syntax
import Pinafore.Syntax.Text
import Shapes

funcPNT :: PrecNamedText -> PrecNamedText -> PrecNamedText
funcPNT ta tb = namedTextPrec 6 $ precNamedText 5 ta <> " -> " <> precNamedText 6 tb

funcPNTList :: [PrecNamedText] -> PrecNamedText -> PrecNamedText
funcPNTList [] t = t
funcPNTList (a:aa) t = funcPNT a $ funcPNTList aa t

signatureDocItem :: SyntaxSignature' -> DocItem
signatureDocItem (ValueSyntaxSignature name stype msdef) = ValueSignatureDocItem name (exprShow stype) (isJust msdef)
signatureDocItem (SupertypeConstructorSyntaxSignature name) = SupertypeConstructorSignatureDocItem name

signatureDefDoc :: SyntaxSignature -> DefDoc
signatureDefDoc (MkSyntaxWithDoc doc (MkWithSourcePos _ sig)) = MkDefDoc (signatureDocItem sig) doc

constructorDocItemDocs :: FullName -> FullName -> SyntaxDataConstructor extra -> (DocItem, Docs)
constructorDocItemDocs tname cname (PlainSyntaxConstructor tt _) =
    ( ValuePatternDocItem (pure $ fullNameRef cname) $
      toNamedText $ funcPNTList (fmap exprShowPrec tt) (exprShowPrec tname)
    , mempty)
constructorDocItemDocs tname cname (RecordSyntaxConstructor sigs) =
    (ValuePatternDocItem (pure $ fullNameRef cname) (exprShow tname), lpure $ fmap signatureDefDoc sigs)

constructorDocItem :: FullName -> FullName -> SyntaxDataConstructor extra -> DocItem
constructorDocItem tname cname dcons = fst $ constructorDocItemDocs tname cname dcons

typeParameterDoc :: SyntaxTypeParameter -> DocTypeParameter
typeParameterDoc (PositiveSyntaxTypeParameter q) = CoDocTypeParameter $ exprShow q
typeParameterDoc (NegativeSyntaxTypeParameter p) = ContraDocTypeParameter $ exprShow p
typeParameterDoc (RangeSyntaxTypeParameter p q) = RangeDocTypeParameter (exprShow p) (exprShow q)

typeDocItem :: FullName -> Bool -> [SyntaxTypeParameter] -> Maybe NamedText -> DocItem
typeDocItem name diStorable tparams mgds = let
    diNames = pure $ fullNameRef name
    diParams = fmap typeParameterDoc tparams
    diGDS = fmap (\gds -> gds <> concatmap (\p -> " " <> exprShow p) tparams) mgds
    in TypeDocItem {..}

typeDeclDoc :: FullName -> SyntaxTypeDeclaration -> RawMarkdown -> Tree DefDoc
typeDeclDoc mtname defn docDescription = let
    consDoc :: FullName -> SyntaxConstructorOrSubtype extra -> (DocItem, Docs)
    consDoc tname (ConstructorSyntaxConstructorOrSubtype cname constructor) =
        constructorDocItemDocs tname cname constructor
    consDoc _ (SubtypeSyntaxConstructorOrSubtype tname tt) =
        (typeDocItem tname storable tparams (Just $ exprShow mtname), typeConssDoc tname tt)
    typeConsDoc :: FullName -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra) -> Tree DefDoc
    typeConsDoc tname (MkSyntaxWithDoc cdoc scs) = let
        (item, rest) = consDoc tname scs
        in MkTree (MkDefDoc item cdoc) rest
    typeConssDoc :: FullName -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] -> Docs
    typeConssDoc tname sdocs = MkForest $ fmap (typeConsDoc tname) sdocs
    (storable, tparams, items) =
        case defn of
            StorableDatatypeSyntaxTypeDeclaration tparams' conss -> (True, tparams', typeConssDoc mtname conss)
            PlainDatatypeSyntaxTypeDeclaration tparams' _ conss -> (False, tparams', typeConssDoc mtname conss)
            _ -> (False, mempty, mempty)
    docItem = typeDocItem mtname storable tparams Nothing
    in MkTree MkDefDoc {..} items

valueDocItem :: ExprShow vtype => FullName -> Maybe vtype -> DocItem
valueDocItem name stype = let
    diNames = pure $ fullNameRef name
    diType = fromMaybe "" $ fmap exprShow stype
    in ValueDocItem {..}

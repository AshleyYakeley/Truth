module Pinafore.Language.Syntax where

import Pinafore.Base
import Pinafore.Language.EntityType
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Read.RefNotation
import Pinafore.Language.Type
import Shapes

newtype TypeDecls baseedit =
    MkTypeDecls (forall a. RefNotation baseedit a -> RefNotation baseedit a)

instance Semigroup (TypeDecls baseedit) where
    (MkTypeDecls a) <> (MkTypeDecls b) = MkTypeDecls (a . b)

instance Monoid (TypeDecls baseedit) where
    mempty = MkTypeDecls id
    mappend = (<>)

data SyntaxDeclarations baseedit =
    MkSyntaxDeclarations (PinaforeScoped baseedit (TypeDecls baseedit))
                         [SyntaxBinding baseedit]

instance Semigroup (SyntaxDeclarations baseedit) where
    (MkSyntaxDeclarations ta ba) <> (MkSyntaxDeclarations tb bb) = MkSyntaxDeclarations (ta <> tb) (ba <> bb)

instance Monoid (SyntaxDeclarations baseedit) where
    mempty = MkSyntaxDeclarations mempty mempty
    mappend = (<>)

typeSyntaxDeclarations :: PinaforeScoped baseedit (TypeDecls baseedit) -> SyntaxDeclarations baseedit
typeSyntaxDeclarations td = MkSyntaxDeclarations td mempty

data SyntaxBinding baseedit =
    MkSyntaxBinding SourcePos
                    Name
                    [SyntaxPattern]
                    (SyntaxExpression baseedit)

data SyntaxPattern =
    MkSyntaxPattern Name

data SyntaxExpression' baseedit
    = SEConst (QValue baseedit)
    | SEVar Name
    | SEApply (SyntaxExpression baseedit)
              [SyntaxExpression baseedit]
    | SEAbstract [SyntaxPattern]
                 (SyntaxExpression baseedit)
    | SERef (SyntaxExpression baseedit)
    | SEUnref (SyntaxExpression baseedit)
    | SELet (SyntaxDeclarations baseedit)
            (SyntaxExpression baseedit)
    | SEList [SyntaxExpression baseedit]
    | SEProperty (PinaforeScoped baseedit (AnyW EntityType))
                 (PinaforeScoped baseedit (AnyW EntityType))
                 Anchor
    | SEEntity (PinaforeScoped baseedit (AnyW EntityType))
               Anchor

data SyntaxExpression baseedit =
    MkSyntaxExpression SourcePos
                       (SyntaxExpression' baseedit)

data SyntaxTopDeclarations baseedit =
    MkSyntaxTopDeclarations SourcePos
                            (SyntaxDeclarations baseedit)

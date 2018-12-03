module Pinafore.Language.TypeContext
    ( NamedType(..)
    , PinaforeTypeCheck
    , runPinaforeTypeCheck
    , SourcePos
    , SourcePinaforeTypeCheck
    , runSourcePos
    , liftSourcePos
    , mapSourcePos
    , runSourcePinaforeTypeCheck
    , convertFailure
    , lookupNamedType
    , withNewTypeName
    , withEntitySubtype
    , getEntitySubtype
    , TypeCheckSubtype(..)
    ) where

import Pinafore.Language.Name
import Pinafore.Language.NamedEntity
import Shapes
import Text.Parsec (SourcePos)

data NamedType =
    EntityNamedType (AnyW SymbolWitness)

data TypeContext = MkTypeContext
    { tcNames :: LazyMap Name NamedType
    , tcEntitySubtypes :: [(Name, Name)]
    }

newtype PinaforeTypeCheck a =
    MkPinaforeTypeCheck (ReaderT TypeContext (Result Text) a)
    deriving (Functor, Applicative, Monad, MonadFail)

instance Semigroup a => Semigroup (PinaforeTypeCheck a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (PinaforeTypeCheck a) where
    mappend = (<>)
    mempty = pure mempty

runPinaforeTypeCheck :: PinaforeTypeCheck a -> Result Text a
runPinaforeTypeCheck (MkPinaforeTypeCheck qa) = runReaderT qa $ MkTypeContext mempty mempty

pTypeContext :: PinaforeTypeCheck TypeContext
pTypeContext = MkPinaforeTypeCheck ask

pLocalTypeContext :: (TypeContext -> TypeContext) -> PinaforeTypeCheck a -> PinaforeTypeCheck a
pLocalTypeContext maptc (MkPinaforeTypeCheck ma) = MkPinaforeTypeCheck $ local maptc ma

newtype SourcePinaforeTypeCheck a =
    MkSourcePinaforeTypeCheck (ReaderT SourcePos PinaforeTypeCheck a)
    deriving (Functor, Applicative, Monad)

runSourcePos :: SourcePos -> SourcePinaforeTypeCheck a -> PinaforeTypeCheck a
runSourcePos spos (MkSourcePinaforeTypeCheck ma) = runReaderT ma spos

liftSourcePos :: PinaforeTypeCheck a -> SourcePinaforeTypeCheck a
liftSourcePos ma = MkSourcePinaforeTypeCheck $ lift ma

mapSourcePos ::
       SourcePos
    -> (SourcePinaforeTypeCheck a -> SourcePinaforeTypeCheck b)
    -> PinaforeTypeCheck a
    -> PinaforeTypeCheck b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

runSourcePinaforeTypeCheck :: SourcePos -> SourcePinaforeTypeCheck a -> Result Text a
runSourcePinaforeTypeCheck spos spa = runPinaforeTypeCheck $ runSourcePos spos spa

spTypeContext :: SourcePinaforeTypeCheck TypeContext
spTypeContext = MkSourcePinaforeTypeCheck $ lift pTypeContext

spLocalTypeContext :: (TypeContext -> TypeContext) -> SourcePinaforeTypeCheck a -> SourcePinaforeTypeCheck a
spLocalTypeContext maptc (MkSourcePinaforeTypeCheck ma) =
    MkSourcePinaforeTypeCheck $ remonad (pLocalTypeContext maptc) ma

instance MonadFail SourcePinaforeTypeCheck where
    fail s =
        MkSourcePinaforeTypeCheck $ do
            spos <- ask
            lift $ fail $ show spos <> ": " <> s

convertFailure :: String -> String -> SourcePinaforeTypeCheck a
convertFailure sa sb = fail $ "cannot convert " <> sa <> " to " <> sb

lookupNamedType :: Name -> SourcePinaforeTypeCheck NamedType
lookupNamedType name = do
    (tcNames -> names) <- spTypeContext
    case lookup name names of
        Just nt -> return nt
        Nothing -> fail $ "unknown type: " <> unpack name

getImmediateSupertypes :: Eq a => [(a, b)] -> a -> [b]
getImmediateSupertypes st a0 = [(b) | (a, b) <- st, a == a0]

expandSupertypes :: Eq a => [(a, a)] -> [a] -> [a]
expandSupertypes st aa = nub $ mconcat $ aa : fmap (getImmediateSupertypes st) aa

isSupertype :: Eq a => [(a, a)] -> [a] -> a -> Bool
isSupertype _st aa a
    | elem a aa = True
isSupertype st aa a = let
    aa' = expandSupertypes st aa
    in if length aa' > length aa
           then isSupertype st aa' a
           else False

castNamedEntity :: NamedEntity na -> NamedEntity nb
castNamedEntity (MkNamedEntity p) = MkNamedEntity p

withNewTypeName :: Name -> NamedType -> SourcePinaforeTypeCheck a -> SourcePinaforeTypeCheck a
withNewTypeName s t ma = spLocalTypeContext (\tc -> tc {tcNames = insertMap s t (tcNames tc)}) ma

withEntitySubtype :: (Name, Name) -> SourcePinaforeTypeCheck a -> SourcePinaforeTypeCheck a
withEntitySubtype rel@(a, b) ma = do
    _ <- lookupNamedType a
    _ <- lookupNamedType b
    spLocalTypeContext (\tc -> tc {tcEntitySubtypes = rel : (tcEntitySubtypes tc)}) ma

getEntitySubtype :: SymbolWitness na -> SymbolWitness nb -> SourcePinaforeTypeCheck (NamedEntity na -> NamedEntity nb)
getEntitySubtype wa wb = let
    sa = fromSymbolWitness wa
    sb = fromSymbolWitness wb
    in do
           (tcEntitySubtypes -> subtypes) <- spTypeContext
           if isSupertype subtypes [fromString sa] (fromString sb)
               then return castNamedEntity
               else convertFailure sa sb

class TypeCheckSubtype w where
    getSubtype :: w a -> w b -> SourcePinaforeTypeCheck (a -> b)

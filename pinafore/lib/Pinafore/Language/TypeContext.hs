module Pinafore.Language.TypeContext
    ( NamedType(..)
    , PinaforeTypeCheck
    , runPinaforeTypeCheck
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

convertFailure :: String -> String -> PinaforeTypeCheck a
convertFailure sa sb = fail $ "cannot convert " <> sa <> " to " <> sb

lookupNamedType :: Name -> PinaforeTypeCheck NamedType
lookupNamedType name = do
    names <- MkPinaforeTypeCheck $ asks tcNames
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

withNewTypeName :: Name -> NamedType -> PinaforeTypeCheck a -> PinaforeTypeCheck a
withNewTypeName s t (MkPinaforeTypeCheck ma) =
    MkPinaforeTypeCheck $ local (\tc -> tc {tcNames = insertMap s t (tcNames tc)}) ma

withEntitySubtype :: (Name, Name) -> PinaforeTypeCheck a -> PinaforeTypeCheck a
withEntitySubtype rel@(a, b) (MkPinaforeTypeCheck ma) = do
    _ <- lookupNamedType a
    _ <- lookupNamedType b
    MkPinaforeTypeCheck $ local (\tc -> tc {tcEntitySubtypes = rel : (tcEntitySubtypes tc)}) ma

getEntitySubtype :: SymbolWitness na -> SymbolWitness nb -> PinaforeTypeCheck (NamedEntity na -> NamedEntity nb)
getEntitySubtype wa wb = let
    sa = fromSymbolWitness wa
    sb = fromSymbolWitness wb
    in do
           subtypes <- MkPinaforeTypeCheck $ asks tcEntitySubtypes
           if isSupertype subtypes [fromString sa] (fromString sb)
               then return castNamedEntity
               else convertFailure sa sb

class TypeCheckSubtype w where
    getSubtype :: w a -> w b -> PinaforeTypeCheck (a -> b)

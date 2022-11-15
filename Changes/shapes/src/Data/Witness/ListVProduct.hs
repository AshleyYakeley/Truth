module Data.Witness.ListVProduct
    ( MapType
    , emptyMapTypeRefl
    , ListVProduct
    , ListVProductType(..)
    , listVectorIsEmpty
    , ListVType(..)
    , mapListVType
    , mapListMVProduct
    , endoListVProduct
    , listVTypeToVector
    , assembleListVType
    , listVTypeToType
    , listTypeToVType
    , listProductToVProduct
    , listVProductToProduct
    , listVProductGetters
    ) where

import Shapes.Import
import Shapes.Unsafe (unsafeRefl)

type family MapType (f :: ka -> kb) (aa :: [ka]) :: [kb] where
    MapType f '[] = '[]
    MapType f (a ': aa) = f a ': MapType f aa

emptyMapTypeRefl ::
       forall ka kb (f :: ka -> kb) (tt :: [ka]). MapType f tt ~ '[]
    => tt :~: '[]
emptyMapTypeRefl = unsafeRefl @[ka] @tt @'[]

-- retain a little type-safety
type family Typical (tt :: [Type]) :: Type where

mapTypeTypicalRefl :: forall (f :: Type -> Type) (aa :: [Type]). Typical (MapType f aa) :~: f (Typical aa)
mapTypeTypicalRefl = unsafeRefl

typicalHeadRefl :: forall (a :: Type) (aa :: [Type]). Typical (a ': aa) :~: a
typicalHeadRefl = unsafeRefl

typicalTailRefl :: forall (a :: Type) (aa :: [Type]). Typical (a ': aa) :~: Typical aa
typicalTailRefl = unsafeRefl

newtype ListVProduct (tt :: [Type]) =
    MkListVProduct (Vector (Typical tt))

listVectorIsEmpty :: forall (tt :: [Type]). ListVProduct tt -> Maybe (tt :~: '[])
listVectorIsEmpty (MkListVProduct v) =
    if length v == 0
        then Just (unsafeRefl @[Type] @tt @'[])
        else Nothing

instance Eq (ListVProduct '[]) where
    MkListVProduct _ == MkListVProduct _ = True

instance Countable (ListVProduct '[]) where
    countPrevious _ = Nothing
    countMaybeNext Nothing = Just single
    countMaybeNext (Just _) = Nothing

instance Searchable (ListVProduct '[]) where
    search = finiteSearch

instance AtLeastOneCountable (ListVProduct '[]) where
    countFirst = single

instance Finite (ListVProduct '[]) where
    allValues = [single]
    assemble afb = liftA (\v _ -> v) (afb single)

instance Singular (ListVProduct '[]) where
    single = MkListVProduct mempty

type ListVType :: (k -> Type) -> ([k] -> Type)
newtype ListVType w tt =
    MkListVType (ListVProduct (MapType w tt))

mapListVProduct ::
       forall (wa :: Type -> Type) (wb :: Type -> Type) (tt :: [Type]).
       (wa --> wb)
    -> ListVProduct (MapType wa tt)
    -> ListVProduct (MapType wb tt)
mapListVProduct f (MkListVProduct v) =
    case mapTypeTypicalRefl @wa @tt of
        Refl ->
            case mapTypeTypicalRefl @wb @tt of
                Refl -> MkListVProduct $ fmap f v

mapListMVProduct ::
       forall m (wa :: Type -> Type) (wb :: Type -> Type) (tt :: [Type]). Applicative m
    => (forall t. wa t -> m (wb t))
    -> ListVProduct (MapType wa tt)
    -> m (ListVProduct (MapType wb tt))
mapListMVProduct f (MkListVProduct v) =
    case mapTypeTypicalRefl @wa @tt of
        Refl ->
            case mapTypeTypicalRefl @wb @tt of
                Refl -> fmap MkListVProduct $ traverse f v

mapListVType ::
       forall (wa :: Type -> Type) (wb :: Type -> Type) (tt :: [Type]).
       (wa --> wb)
    -> ListVType wa tt
    -> ListVType wb tt
mapListVType f (MkListVType lv) = MkListVType $ mapListVProduct @wa @wb @tt f lv

type ListVProductType :: (Type -> Type) -> (Type -> Type)
data ListVProductType w t where
    MkListVProductType
        :: forall (w :: Type -> Type) (tt :: [Type]). ListVType w tt -> ListVProductType w (ListVProduct tt)

endoListVProduct :: forall tt. ListVProduct (MapType Endo tt) -> Endo (ListVProduct tt)
endoListVProduct (MkListVProduct vf) =
    case mapTypeTypicalRefl @Endo @tt of
        Refl -> Endo $ \(MkListVProduct va) -> MkListVProduct $ zipWith (\(Endo f) a -> f a) vf va

listVTypeToVector :: forall (w :: Type -> Type) t r. (forall a. w a -> r) -> ListVType w t -> Vector r
listVTypeToVector wr (MkListVType (MkListVProduct v)) =
    case mapTypeTypicalRefl @w @t of
        Refl -> fmap wr v

type family AList :: [Type] where

typicalAList :: forall (a :: Type). Typical AList :~: a
typicalAList = unsafeRefl

listEmptyRefl :: forall (tt :: [Type]). tt :~: '[]
listEmptyRefl = unsafeRefl

listConsRefl :: forall (tt :: [Type]). tt :~: (Typical tt ': TTRest tt)
listConsRefl = unsafeRefl

type family TTRest (tt :: [Type]) :: [Type] where

typicalRestRefl :: forall (tt :: [Type]). Typical (TTRest tt) :~: Typical tt
typicalRestRefl =
    case listConsRefl @tt of
        Refl ->
            case typicalTailRefl @(Typical tt) @(TTRest tt) of
                Refl -> Refl

assembleListVType :: forall (w :: Type -> Type). Vector (Some w) -> Some (ListVType w)
assembleListVType v =
    case mapTypeTypicalRefl @w @AList of
        Refl ->
            MkSome $
            MkListVType @Type @w @AList $
            MkListVProduct $
            fmap
                (\(MkSome (wa :: w a)) ->
                     case typicalAList @a of
                         Refl -> wa)
                v

mkTypicalListType :: forall (w :: Type -> Type) (tt :: [Type]). [w (Typical tt)] -> ListType w tt
mkTypicalListType [] =
    case listEmptyRefl @tt of
        Refl -> NilListType
mkTypicalListType (wt:wtt) =
    case (listConsRefl @tt, typicalRestRefl @tt) of
        (Refl, Refl) -> ConsListType wt $ mkTypicalListType @w @(TTRest tt) wtt

listVTypeToType :: forall (w :: Type -> Type) (tt :: [Type]). ListVType w tt -> ListType w tt
listVTypeToType (MkListVType (MkListVProduct lvt)) =
    case mapTypeTypicalRefl @w @tt of
        Refl -> mkTypicalListType @w @tt $ toList lvt

listTypeTypical :: forall w tt. ListType w tt -> [w (Typical tt)]
listTypeTypical NilListType = []
listTypeTypical (ConsListType (a :: _ t1) (aa :: _ tr)) =
    case (typicalHeadRefl @t1 @tr, typicalTailRefl @t1 @tr) of
        (Refl, Refl) -> a : listTypeTypical aa

listTypeToVType :: forall (w :: Type -> Type) (tt :: [Type]). ListType w tt -> ListVType w tt
listTypeToVType lt =
    case mapTypeTypicalRefl @w @tt of
        Refl -> MkListVType $ MkListVProduct $ fromList $ listTypeTypical lt

listProductToTypicalList :: forall (tt :: [Type]) a. [a] -> ListProduct tt -> [Typical tt]
listProductToTypicalList [] =
    case listEmptyRefl @tt of
        Refl -> \() -> []
listProductToTypicalList (_:aa) =
    case (listConsRefl @tt, typicalRestRefl @tt) of
        (Refl, Refl) -> \(t, tt) -> t : listProductToTypicalList aa tt

listProductToVProduct :: forall (tt :: [Type]) w. ListVType w tt -> ListProduct tt -> ListVProduct tt
listProductToVProduct (MkListVType (MkListVProduct lvt)) =
    case mapTypeTypicalRefl @w @tt of
        Refl -> \lp -> MkListVProduct $ fromList $ listProductToTypicalList (toList lvt) lp

typicalListToListProduct :: forall (tt :: [Type]). [Typical tt] -> ListProduct tt
typicalListToListProduct [] =
    case listEmptyRefl @tt of
        Refl -> ()
typicalListToListProduct (a:aa) =
    case (listConsRefl @tt, typicalRestRefl @tt) of
        (Refl, Refl) -> (a, typicalListToListProduct aa)

listVProductToProduct :: forall (tt :: [Type]). ListVProduct tt -> ListProduct tt
listVProductToProduct (MkListVProduct v) = typicalListToListProduct $ toList v

getItem :: Int -> ListVProduct tt -> Typical tt
getItem i (MkListVProduct v) = indexEx v i

getters :: (Typical tt ~ Typical tt') => Int -> ListType w tt -> ListType ((->) (ListVProduct tt')) tt
getters _ NilListType = NilListType
getters i (ConsListType (_ :: _ a) (tt :: _ aa)) =
    case (typicalHeadRefl @a @aa, typicalTailRefl @a @aa) of
        (Refl, Refl) -> ConsListType (getItem i) $ getters (succ i) tt

listVProductGetters :: forall (tt :: [Type]) w. ListType w tt -> ListType ((->) (ListVProduct tt)) tt
listVProductGetters = getters 0

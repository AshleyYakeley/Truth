module UI.Truth.GTK.Tuple where
{
	import Data.Changes;
	import Data.Witness;
	import Data.Traversable;
	import Data.TypeFunc;

	type family ListMap tf l;
	type instance ListMap tf () = ();
	type instance ListMap tf (h,r) = (TF tf h,ListMap tf r);

	mapListElement :: Type tf -> ListElementType l elem -> ListElementType (ListMap tf l) (TF tf elem);
	mapListElement _ HeadListElementType = HeadListElementType;
	mapListElement tf (TailListElementType et) = TailListElementType (mapListElement tf et);

	makeListMap :: Type tf -> ListType wit l -> (forall elem. wit elem -> ListElementType l elem -> TF tf elem) -> ListMap tf l;
	makeListMap _tf NilListType _f = ();
	makeListMap tf (ConsListType wit lt) f = (f wit HeadListElementType,makeListMap tf lt (\wit' elemwit -> f wit' (TailListElementType elemwit)));

	makeListMapM :: (Monad m) =>
	 Type tf -> ListType wit l -> (forall elem. wit elem -> ListElementType l elem -> m (TF tf elem)) -> m (ListMap tf l);
	makeListMapM _tf NilListType _f = return ();
	makeListMapM tf (ConsListType wit lt) f = do
	{
		h <- f wit HeadListElementType;
		t <- makeListMapM tf lt (\wit' elemwit -> f wit' (TailListElementType elemwit));
		return (h,t);
	};

	data EditableWit a where
	{
		MkEditableWit :: (Editable a) => EditableWit a;
	};

	data TFView w;
	type instance TF (TFView w) a = View w a;
	data TFViewResult w;
	type instance TF (TFViewResult w) a = ViewResult w a;

	tupleView :: forall a w. (IsTuple a, PartEdit a ~ TListPartEdit (TList a)) =>
	 ListType EditableWit (TList a) -> ([w] -> IO w) -> ListMap (TFView w) (TList a) -> View w a;
	tupleView editablewit aggregate  views ia pusha = let
	{
		tfView :: Type (TFView w);
		tfView = Type;
		tfViewResult :: Type (TFViewResult w);
		tfViewResult = Type;
	} in do
	{
		vrs :: ListMap (TFViewResult w) (TList a)
		 <- makeListMapM tfViewResult editablewit (\MkEditableWit elemwit -> let
		{
			view = getListElement (mapListElement tfView elemwit) views;
			ielem = getListElement elemwit (toListTuple ia);
			pushelem editelem = pusha (PartEdit (TListPartEdit elemwit editelem));
		} in view ielem pushelem);
		let
		{
			getVR :: ListElementType (TList a) elem -> ViewResult w elem;
			getVR elemwit = getListElement (mapListElement tfViewResult elemwit) vrs;
		
			pickWidget :: AnyF EditableWit (ListElementType (TList a)) -> w;
			pickWidget (MkAnyF _ elemwit) = vrWidget (getVR elemwit);
		};
		iow <- aggregate (fmap pickWidget (getListElementTypes editablewit));
		return (MkViewResult iow (\edita -> case edita of
		{
			ReplaceEdit a -> (let
			{
				tla = toListTuple a;
			} in forM (getListElementTypes editablewit)
			 (\(MkAnyF _ elemwit) -> vrUpdate (getVR elemwit) (ReplaceEdit (getListElement elemwit tla)))
			) >> return ();
			PartEdit (TListPartEdit elemwit edite) -> vrUpdate (getVR elemwit) edite;
		})); 
	};

}

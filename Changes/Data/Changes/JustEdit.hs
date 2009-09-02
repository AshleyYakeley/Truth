module Data.Changes.JustEdit where
{
	import Data.Changes.EditScheme;
	import Data.Changes.HasTypeRep;
	import Data.FunctorOne;
	import Data.Result;
	import Data.Chain;
	import Data.OpenWitness.OpenRep;
	import Data.OpenWitness;
	import Control.Applicative;
	import Prelude hiding (id,(.));

	data JustEdit a edit = ReplaceJustEdit a | JustEdit edit;

	instance (EditScheme a edit,FunctorOne f) => EditScheme (f a) (JustEdit (f a) edit) where
	{
		applyEdit (ReplaceJustEdit a) = pure a;
		applyEdit (JustEdit edita) = cfmap (applyEdit edita);

		invertEdit (ReplaceJustEdit _) a = Just (ReplaceJustEdit a);
		invertEdit (JustEdit edita) molda = case retrieveOne molda of
        {
	        SuccessResult olda -> fmap JustEdit (invertEdit edita olda);
	        _ -> Nothing;
        };
	};
	
	instance (EditScheme a edit,FunctorOne f) => CompleteEditScheme (f a) (JustEdit (f a) edit) where
	{
	    replaceEdit = ReplaceJustEdit;
	};

	extractJustEdit :: forall f a edit. (FunctorOne f,CompleteEditScheme a edit) => JustEdit (f a) edit -> Maybe edit;
	extractJustEdit (JustEdit edit) = Just edit;
	extractJustEdit (ReplaceJustEdit fa) = case retrieveOne fa of
	{
		SuccessResult a -> Just (replaceEdit a);
		_ -> Nothing;
	};
	
	instance HasTypeRep2 JustEdit where
	{
	    typeRep2 = SimpleOpenRep2 (unsafeIOWitnessFromString "Data.Changes.JustEdit.JustEdit");
	};
}

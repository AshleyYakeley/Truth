module Data.Result where
{
	import Data.Bijection;
	import Control.Monad;

	data Result e a = SuccessResult a | FailureResult e;
	
	instance Functor (Result e) where
	{
		fmap ab (SuccessResult a) = SuccessResult (ab a);
		fmap _ (FailureResult e) = FailureResult e;
	};

	instance Monad (Result e) where
	{
		return = SuccessResult;
		(FailureResult e) >>= _ = FailureResult e;
		(SuccessResult a) >>= amq = amq a;
		fail _ = FailureResult undefined;
	};
	
	mapResult :: Bijection (Result e2 (Result e1 a)) (Result (Either e2 e1) a);
	mapResult = MkBijection forwards backwards where
	{
		forwards (SuccessResult (SuccessResult a)) = SuccessResult a;
		forwards (SuccessResult (FailureResult e1)) = FailureResult (Right e1);
		forwards (FailureResult e2) = FailureResult (Left e2);
		
		backwards (SuccessResult a) = SuccessResult (SuccessResult a);
		backwards (FailureResult (Right e1)) = SuccessResult (FailureResult e1);
		backwards (FailureResult (Left e2)) = FailureResult e2;
	};
}

module Control.Concurrent.STM.TLock(TLock,withTLock,newTLock,newTLockIO) where
{
	import Control.Concurrent.STM;

	type TLock = TVar Bool;

	lockTLock :: TLock -> STM a -> STM a;
	lockTLock lock action = do
	{
		locked <- readTVar lock;
		if locked
		 then retry
		 else do
		{
			a <- action;
			writeTVar lock True;
			return a;
		};
	};

	unlockTLock :: TLock -> STM a -> STM a;
	unlockTLock lock action = do
	{
		writeTVar lock False;
		action;
	};

	withTLock :: TLock -> STM a -> (a -> IO b) -> (b -> STM c) -> IO c;
	withTLock lock sa aib bsc = do
	{
		a <- atomically (lockTLock lock sa);
		b <- aib a;
		atomically (unlockTLock lock (bsc b));
	};

	newTLock :: STM TLock;
	newTLock = newTVar False;

	newTLockIO :: IO TLock;
	newTLockIO = newTVarIO False;
}

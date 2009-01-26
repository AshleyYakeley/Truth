module Control.Wait where
{
	import Control.Concurrent;
	import Control.Exception;
	
	type WaitIO a = (?wait :: MVar ()) => IO a;
	
	interruptW :: (Exception e) => ThreadId -> MVar () -> e -> IO ();
	interruptW thread waitvar ex = withMVar waitvar (\_ -> throwTo thread ex);
	
	-- note that blocking MVar operations can always be interrupted, even inside a block
	
	forkWaitIO :: WaitIO () -> IO (forall e. (Exception e) => e -> IO ());
	forkWaitIO wio = block (do
	{
		waitvar <- newMVar ();
		thread <- forkIO (let {?wait = waitvar} in wio);
		return (interruptW thread waitvar);
	});
	
	yieldW :: WaitIO ();
	yieldW = readMVar ?wait;
}

module Control.Wait where
{
    import Control.Concurrent;
    import Control.Exception;


    type WaitIO a = (?wait :: MVar ()) => IO a;

    newtype Throw = MkThrow (forall e. (Exception e) => e -> IO ());

    interruptW :: ThreadId -> MVar () -> Throw;
    interruptW thread waitvar = MkThrow (\ex -> withMVar waitvar (\_ -> throwTo thread ex));

    -- note that blocking MVar operations can always be interrupted, even inside a block

    forkWaitIO :: WaitIO () -> IO Throw;
    forkWaitIO wio = mask (\_ -> do
    {
        waitvar <- newMVar ();
        thread <- forkIO (let {?wait = waitvar} in wio);
        return (interruptW thread waitvar);
    });

    yieldW :: WaitIO ();
    yieldW = readMVar ?wait;
}

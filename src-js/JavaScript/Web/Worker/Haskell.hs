{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI  #-}

{- | Haskell-specific web worker API. The URL is expected to point to a script
     that is the same as the caller, or at least a script that has been
     produced by GHCJS and contains the same static values.
 -}

module JavaScript.Web.Worker.Haskell ( HaskellWorker
                                     , terminate
                                     , call
                                     ) where


import qualified JavaScript.Web.Worker as W

data HaskellWorker = HaskellWorker W.Worker

create :: JSString -> IO HaskellWorker
create uri = fmap HaskellWorker (W.create uri)
{-# INLINE create #-}

-- fixme stop all waiters?
terminate :: HaskellWorker -> IO ()
terminate (HaskellWorker w) = W.terminate w
{-# INLINE terminate #-}

-- call :: SomethingSomething -> HaskellWorker -> IO a
call hw = undefined
{-# INLINE call #-}

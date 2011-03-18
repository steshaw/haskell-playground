--
-- This code was found at http://www.haskell.org/haskellwiki/WrapConc
--
-- Unfortunately, doesn't yet compile with GHC 7.0.2.
--

--
-- By Chris Kuklewicz <haskell at list dot mightyreason dot com>
--
-- This WrapConc module provides modified version of forkIO which have
-- been combined with block from Control.Exception to provide stronger
-- guarantees as well as notification when a thread has finished.
--
module WrapConc(finally',bracket',fork,forkB,forkC,forkF
               ,ThreadFlag,isSetThreadFlag,waitThreadFlag,waitKillThread,abandonThread) where
 
import Prelude hiding (catch)
import Control.Concurrent(forkIO,ThreadId,killThread,yield)
import Control.Concurrent.MVar(MVar,newEmptyMVar,tryPutMVar,takeMVar,putMVar,isEmptyMVar,readMVar)
import Control.Exception(Exception,catch,block,throw)
import Control.Monad (liftM)
 
-- Some code copied from http://darcs.haskell.org/packages/base/Control/Exception.hs
 
-- This is like finally but leaves your code to execute in 'block'
-- instead of 'unblock'
finally' :: IO a -> IO b -> IO a
finally' a sequel = block $ do
  r <- catch a (\e -> sequel >> throw e)
  sequel
  return r
 
-- This is like bracket but leaves your code to execute in 'block'
-- instead of 'unblock'
bracket' :: IO a         -- ^ computation to run first (\"acquire resource\")
         -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
         -> (a -> IO c)  -- ^ computation to run in-between
         -> IO c         -- returns the value from the in-between computation
bracket' before after thing = block $ do
  a <- before 
  r <- catch (thing a) (\e -> after a >> throw e )
  after a
  return r
 
-- ThreadFlag is a limited newtype of an MVar
newtype ThreadFlag = ThreadFlag (MVar ())
 
-- Intially unset, the ThreadFlag will become set when the thread is finished
isSetThreadFlag :: ThreadFlag -> IO Bool
isSetThreadFlag (ThreadFlag m) = liftM not (isEmptyMVar m)
 
-- If unset then block until set.  This will wait for the thread to have finished
waitThreadFlag :: ThreadFlag -> IO ()
waitThreadFlag (ThreadFlag m) = readMVar m
 
-- This does a background killThread and then waits for the passed
-- ThreadFlag to signal that it is done via the ThreadFlag
waitKillThread :: (ThreadFlag,ThreadId) -> IO ()
waitKillThread (flag,tid) = forkIO (killThread tid) >> yield >> waitThreadFlag flag
 
-- This does two things:
--   First it does a background killThread.
--   Second it immediately ensure the ThreadFlag is set
--
-- The main effect will be to release any thread waiting on (or that
-- will wait on) the ThreadFlag.
--
-- This should only be needed in highly unusual situations where
-- waiting for thread to properly exit and set the ThreadFlag is
-- impossible.  It may be useful if the thread has hung or the
-- application needs to perform an "emergency shutdown".
abandonThread (ThreadFlag m,tid) = forkIO (killThread tid) >> tryPutMVar m ()
 
-- Safer forkIO. If this returns then you are not guaranteed that your
-- operations run but you are guranteed that the thread has started
-- and that the ThreadFlag will be set when it finishes.  Your code
-- starts to execute in the scope of a 'block' function.
--
-- It is possible to interrupt the fork and to have still succeeded in
-- starting the thread.
fork :: IO a -> IO (ThreadFlag,ThreadId)
fork ioA = block $ do
  a <- newEmptyMVar
  b <- newEmptyMVar
  tid <- forkIO (finally' (putMVar a () >> yield >> ioA)
                          (tryPutMVar b ()) >> return ())
  yield
  takeMVar a
  return (ThreadFlag b, tid)
 
-- fork merged with bracket'. If this returns then you are not
-- guaranteed that your operations run but you are guranteed that the
-- thread has started and that the ThreadFlag will be set when it
-- finishes.  Your code starts to execute in the scope of a 'block'
-- function.
--
-- The is equivalent to (fork (bracket' ioA ioAB ioAC))
--
-- It is possible to interrupt the forkB and to have still succeeded in
-- starting the thread.
forkB :: IO a -> (a -> IO b) -> (a -> IO c) -> IO (ThreadFlag,ThreadId)
forkB ioA ioAB ioAC = block $ do
  a <- newEmptyMVar
  b <- newEmptyMVar
  tid <- forkIO (finally' (putMVar a () >> yield >> bracket' ioA ioAB ioAC)
                          (tryPutMVar b ()) >> return ())
  yield
  takeMVar a
  return (ThreadFlag b, tid)
 
-- fork merged with catch. If this returns then you are not guarenteed
-- that your operation is running, but you are guranteed that the
-- handler will run if your code received or raises an exception. And
-- the ThreadFlag will be set when the thread finishes.  Your code
-- starts to execute in the scope of a 'block' function.
--
-- This is a stronger guarantee than (fork (catch ioA ioEA))
--
-- It is possible to interrupt the forkC and to have still succeeded in
-- starting the thread.
forkC :: IO a -> (Exception -> IO a) -> IO (ThreadFlag,ThreadId)
forkC ioA ioEA = block $ do
  a <- newEmptyMVar
  b <- newEmptyMVar
  tid <- forkIO (finally' (catch (putMVar a () >> yield >> ioA) ioEA)
                          (tryPutMVar b ()) >> return ())
  yield
  takeMVar a
  return (ThreadFlag b, tid)
 
-- fork merged with finally'. If this returns then you are guaranteed
-- that your second command will be run even if your first command
-- receives or raises an exception. And the ThreadFlag will be set
-- when the thread finishes.  Your code starts to execute in the
-- scope of a 'block' function.
--
-- This is a stronger guarantee than (fork (finally' ioA ioB))
--
-- It is possible to interrupt the forkF and to have still succeeded in
-- starting the thread.
forkF :: IO a -> IO b -> IO (ThreadFlag,ThreadId)
forkF ioA ioB = block $ do
  a <- newEmptyMVar
  b <- newEmptyMVar
  tid <- forkIO (finally' (finally' (putMVar a () >> yield >> ioA) ioB)
                          (tryPutMVar b ()) >> return ())
  yield
  takeMVar a
  return (ThreadFlag b, tid)

module Admission (Admission, newAdmission, withAdmission, closeAdmission, openAdmission, admissionOpen) where

import Control.Concurrent.STM
import Control.Exception

data Phase = Preparing | Open | Closed deriving Eq
newtype Admission = Admission (TVar (Phase, Int))

newAdmission :: Bool -> IO Admission
newAdmission preparing = Admission <$> newTVarIO (if preparing then Preparing else Open, 0)

withAdmission :: Admission -> Bool -> IO a -> IO (Maybe a)
withAdmission (Admission state) write action = mask $ \restore -> do
 accepted <- atomically $ do
  (phase, count) <- readTVar state
  if phase == Open || phase == Preparing && not write
   then writeTVar state (phase, count + 1) >> pure True
   else pure False
 if not accepted then pure Nothing else
  Just <$> restore action `finally` atomically (modifyTVar' state (\(phase,count) -> (phase,count-1)))

closeAdmission :: Admission -> IO ()
closeAdmission (Admission state) = do
 atomically $ modifyTVar' state (\(_,count) -> (Closed,count))
 atomically $ readTVar state >>= check . (==0) . snd

openAdmission :: Admission -> IO ()
openAdmission (Admission state) = atomically $ modifyTVar' state (\(_,count) -> (Open,count))

admissionOpen :: Admission -> IO Bool
admissionOpen (Admission state) = (==Open) . fst <$> readTVarIO state

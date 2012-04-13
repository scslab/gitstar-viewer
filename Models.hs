module Models ( CommitObj(..)
              , getCommitObj
              ) where

import LIO.DCLabel
import Gitstar.Repo

-- | Commit object and stats
data CommitObj = CommitObj { commitObj   :: GitCommit
                             -- ^ Git commit object
                           , commitStats :: GitStats
                             -- ^ Stats on commit
                           } deriving (Eq, Show)

-- | Get commit object 
getCommitObj :: Repo -> SHA1 -> DC (Maybe CommitObj)
getCommitObj repo sha = do
  mcommit <- getCommit repo sha
  mstats  <- getStats repo sha
  return $ do c <- mcommit 
              s <- mstats 
              return CommitObj { commitObj = c, commitStats = s }

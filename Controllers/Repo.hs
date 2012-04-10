{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Repo ( showTreeOrCommit
                        , showBlob
                        , showMasterBranch
                        ) where

import Layouts
import Views.Repo
import Utils

import LIO
import LIO.DCLabel

import Gitstar.Repo

import qualified Data.List as List
import Data.Maybe
import Data.IterIO.Http
import Data.IterIO.Http.Support
import qualified Data.ByteString.Char8 as S8

import System.FilePath (splitDirectories)

-- | Redirect to show master branch
showMasterBranch :: Action t b DC ()
showMasterBranch = do
  uName <- getParamVal "user_name"
  pName <- getParamVal "project_name"
  redirectTo $ "/" ++ uName ++ "/" ++ pName ++ "/tree/master"


--
-- Tree / commit
--

-- | Find the branch/tree and and diplay the corresponding
-- commit/tree.
showTreeOrCommit :: Action t b DC ()
showTreeOrCommit = do
  uName <- getParamVal "user_name"
  pName <- getParamVal "project_name"
  bName <- getParamVal "id" -- tree sha or branch name
  req   <- getHttpReq
  let path = S8.unpack . reqPath $ req
      dirs = drop 3 $ splitDirectories' path -- rm /usr/proj/tree
      repo = Repo { repoOwner = uName, repoName = pName }
  mbranches <- liftLIO $ getBranches repo
  with404orJust mbranches $ \bs ->
    if null bs
      then respond404
      else case List.lookup bName bs of
            Just sha -> commitShow repo sha dirs
            _        -> treeShow repo (SHA1 bName) dirs Nothing 
                          

-- | Given a title and refernce to the commit, find the commit object
-- and tree it points to, and show them.
commitShow :: Repo -> SHA1 -> [String] -> Action t b DC ()
commitShow repo sha dirs = do
  mcommit <- liftLIO $ getCommit repo sha
  with404orJust mcommit $ \commit ->
    treeShow repo (cmtTree commit) dirs (Just commit)

-- | Show a tree
treeShow :: Repo -> SHA1 -> [String] -> Maybe GitCommit -> Action t b DC ()
treeShow repo sha dirs mcommit = do
  mtree <- liftLIO $ getTree repo sha
  with404orJust mtree $ \tree -> do
    mt <- liftLIO $ getTreeByPath repo tree $ tail dirs
    with404orJust mt $ \t -> 
      renderHtml $ viewTreeOrCommit repo sha (if length dirs == 1
                                                then mcommit
                                                else Nothing) t dirs

-- | Find the tree given a head tree object and remaining tree names
getTreeByPath :: Repo -> GitTree -> [String] -> DC (Maybe GitTree)
getTreeByPath repo headTree dirs = 
  if null dirs
    then return $ Just headTree
    else do let n = head dirs
                newPath = safeTail dirs
                mEnt = listToMaybe $ filter ((==n) . entPath) headTree
            case mEnt of
              Nothing -> return Nothing
              Just ent -> do mtree <- getTree repo (entPtr ent)
                             case mtree of
                               Nothing -> return Nothing
                               Just tree -> getTreeByPath repo tree newPath


--
-- Blobs
--

showBlob :: Action t b DC ()
showBlob = do
  uName <- getParamVal "user_name"
  pName <- getParamVal "project_name"
  bName <- getParamVal "id" -- tree sha or branch name
  req   <- getHttpReq
  let path = S8.unpack . reqPath $ req
      dirs = drop 3 $ splitDirectories' path -- rm /usr/proj/blob
      objName = last dirs
      repo = Repo { repoOwner = uName, repoName = pName }
  mbranches <- liftLIO $ getBranches repo
  with404orJust mbranches $ \bs ->
    if null bs
      then respond404
      else do let sha = fromMaybe (SHA1 bName) $  List.lookup bName bs
              mtree <- liftLIO $ getTree repo sha
              with404orJust mtree $ \tree -> do
                mt <- liftLIO $ getTreeByPath repo tree (init . tail $ dirs)
                with404orJust mt $ \t -> 
                  let mEnt = listToMaybe $ filter ((==objName) . entPath) t
                  in with404orJust mEnt $ \ent -> do
                       mblob <- liftLIO $ getBlob repo (entPtr ent)
                       with404orJust mblob $ \blob -> 
                        renderHtml $ viewBlob repo sha blob dirs

--
-- Misc
--

-- | Same as 'splitdirectories', but does not keep the first slash.
splitDirectories' :: FilePath -> [FilePath]
splitDirectories' p = let ds = splitDirectories p
                      in case ds of
                          ("/":xs) -> xs
                          xs       -> xs

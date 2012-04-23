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
                        , showBranches
                        , showCommit
                        --
                        , showLint
                        ) where

import Models
import Layouts
import Views.Repo
import Utils

import LIO
import LIO.MonadCatch
import LIO.Handle
import LIO.DCLabel

import Hails.CJail

import Gitstar.Repo

import Control.Monad

import qualified Data.List as List
import Data.Maybe
import Data.IterIO.Http
import Data.IterIO.Http.Support
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

import System.FilePath (splitDirectories, takeExtension)

-- | Redirect to show master branch
showBranches :: Action t b DC ()
showBranches = do
  uName <- getParamVal "user_name"
  pName <- getParamVal "project_name"
  let repo = Repo { repoOwner = uName, repoName = pName }
  mbranches <- liftLIO $ getBranches repo
  renderHtml $ viewMBranches repo mbranches


--
-- Tree / commit tree
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
            Just sha -> treeCommitShow repo sha dirs
            _        -> treeShow repo (SHA1 bName) dirs Nothing 
                          

-- | Given a title and refernce to the commit, find the commit object
-- and tree it points to, and show them.
treeCommitShow :: Repo -> SHA1 -> [String] -> Action t b DC ()
treeCommitShow repo sha dirs = do
  mcommit <- liftLIO $ getCommitObj repo sha
  with404orJust mcommit $ \commit ->
    treeShow repo (cmtTree . commitObj $  commit) dirs (Just commit)

-- | Show a tree
treeShow :: Repo -> SHA1 -> [String] -> Maybe CommitObj -> Action t b DC ()
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
-- Commit objects
--

showCommit :: Action t b DC ()
showCommit = do
  uName <- getParamVal "user_name"
  pName <- getParamVal "project_name"
  sha   <- SHA1 `liftM` getParamVal "id"
  let repo = Repo { repoOwner = uName, repoName = pName }
  mcommit <- liftLIO $ getCommitObj repo sha
  mdiffs  <- liftLIO $ getDiffs repo sha 
  let mcd = mcommit >>= \mc -> mdiffs >>= \md -> return (mc,md)
  with404orJust mcd $ \(commit, diffs) -> 
    renderHtml $ viewCommit repo commit diffs



--
-- Blobs
--

-- | Generic blob show (view function may perform some filtering)
showBlobGen :: (Repo -> SHA1 -> GitBlob -> [FilePath] -> Action t b DC ()) 
            -> Action t b DC ()
showBlobGen viewBlobFunc = do
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
                        viewBlobFunc repo sha blob dirs

showBlob :: Action t b DC ()
showBlob = showBlobGen (\repo sha blob dirs ->
                        renderHtml $ viewBlob repo sha blob dirs)

showLint :: Action t b DC ()
showLint = showBlobGen (\repo sha blob dirs -> do
  let (lint,tmp) = case takeExtension (last dirs) of
                     ex@(".hs") -> ("hlint", "/tmp/xxx"++ex)
                     ex         -> ("splint","/tmp/xxx"++ex)
  out <- liftLIO $ inCJail' $ do
    lph <- createProcess (shell $ "cat > " ++ tmp ++ " && " ++ lint ++ " " ++ tmp)
    liftLIO $ hPut (stdIn lph) (B64.decodeLenient $ blobContent blob)
    liftLIO $ hClose (stdIn lph)
    resErr <- liftLIO $ hGetContents (stdErr lph)
    res <- liftLIO $ hGetContents (stdOut lph)
    let nres = S8.unlines $ map (stripPrefix . S8.pack $ tmp ++ ":") $ S8.lines res
    return $ if S8.null nres then resErr else nres
  renderHtml $ viewFilteredBlob repo sha blob (lint ++ " output:") out dirs)
    where stripPrefix s ss = if s `S8.isPrefixOf` ss
                              then S8.drop (S8.length s) ss
                              else ss
          inCJail' act = (inCJail act) `onException` return "Execution failed"


--
-- Misc
--

-- | Same as 'splitdirectories', but does not keep the first slash.
splitDirectories' :: FilePath -> [FilePath]
splitDirectories' p = let ds = splitDirectories p
                      in case ds of
                          ("/":xs) -> xs
                          xs       -> xs

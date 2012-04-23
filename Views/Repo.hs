{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Repo ( viewTreeOrCommit
                  , viewBlob
                  , viewCommit
                  , viewMBranches
                  --
                  , viewFilteredBlob 
                  ) where

import Prelude hiding (div, span, id)
import qualified Prelude as Prelude
import Control.Monad

import Data.Char (isSpace)
import Data.Maybe
import qualified Data.List as List
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Base64 as B64

import Text.Blaze.Html5 hiding (title, style, map)
import Text.Blaze.Html5.Attributes hiding (label, form, span, title)

import Gitstar.Repo

import System.FilePath.Posix (takeExtension)
import Hails.IterIO.Mime

import Models
import Utils

import Data.UniDiff

-- | Show main branch selector page
viewMBranches :: Repo -> Maybe [(String, SHA1)] -> Html
viewMBranches repo mbranches = do
  h2 . toHtml $ "Browse " ++ repo2url repo
  case mbranches of
    Just branches | not (null branches) -> do
      let bNames = map fst branches
          master = if "master" `elem` bNames
                     then "master"
                     else List.head bNames
          rest   = List.delete master bNames
      ul ! class_ "nav nav-pills" $ do
        li ! class_ "nav-middle-header" $ "View branch:"
        li ! class_ "dropdown" $ do
          a ! class_ "dropdown-toggle" ! dataAttribute "toggle" "dropdown"
            ! href "#" $ do
            toHtml master
            span ! class_ "caret" $ ""
          ul ! class_ "dropdown-menu" $
            forM_ (master:rest) $ \branch ->
              li $ a ! href (branchUrl branch) $ toHtml branch
    _ -> p . toHtml $ defaultMsg
  where defaultMsg :: String
        defaultMsg = "Project doesn't seem to have any branches." ++
                     " Perhaps you should push to a branch such as \'master\'."
        branchUrl branch = toValue $ repo2url repo ++ "/tree/" ++ branch

-- | Show a commit or tree.
viewTreeOrCommit :: Repo
                 -> SHA1
                 -> Maybe CommitObj
                 -> GitTree
                 -> [String]
                 -> Html
viewTreeOrCommit repo _   mcommit tree dirs = do
  let title = List.head dirs -- branch name
      -- (name, path) pairs for building breadcrumb links
      links = foldl (\paths d -> paths ++ [(d, (snd . List.last) paths ++ "/" ++ d)])
                    [(title, title)] (safeTail dirs)
      -- tree path
      path  = snd . last $ links
  ul ! class_"breadcrumb" $ do
    li $ do a ! href (toValue $ repo2url repo) $
              span ! class_ "icon-home" $ " "
            span ! class_ "divider" $ "/"
    forM_ links $ \(n,lnk) -> 
        li $ do a ! href (toValue $ repo2url repo ++ "/tree/" ++ lnk) $ toHtml n
                span ! class_ "divider" $ "/"
  -- Generate comit box, if any:
  maybe (return ()) (htmlCommitBox repo) mcommit
  table ! class_ "table table-bordered table-condensed" $ do
    colgroup $ do
      col 
      col ! class_ "span2"
    thead ! class_ "curved" $ tr $ do th "name"
                                      th "size"
    tbody $ forM_ tree $ \ent ->
      tr $ do th $ htmlTreeEntry repo ent path
              th $ maybe "--" (toHtml . showSize) $ entSize ent
      where showSize x = let dkb = truncate $ (toRational x) / 1024 * 100 :: Int
                             kb = fromRational $ toRational dkb / 100 :: Double
                         in show kb ++ " KB"

-- | Show a commit object
viewCommit :: Repo -> CommitObj -> [GitDiff] -> Html
viewCommit repo cmtObj diffs = do
  -- Show commit info
  htmlCommitBox repo cmtObj
  -- Show file changes
  let stats  = commitStats cmtObj
  let nrs = [1..] :: [Int]
  table ! class_ "table" $ do
    colgroup $ do
      col ! class_ "span1"
      col 
      col ! class_ "span2"
      col ! class_ "span2"
    forM_ (zip diffs nrs) $ \(diff,nr) -> do
      let path = dpathName . diffPath $ diff
          mstat = getFileStat stats path
      tr $ do
         td $ case dpathChanges (diffPath diff) of
               Just NewFile     -> span ! class_ "icon-plus-sign" $ ""
               Just DeletedFile -> span ! class_ "icon-minus-sign" $ ""
               _                -> span ! class_ "icon-adjust" $ ""
         td $ a ! href (toValue $ "#diff-" ++ show nr) $ toHtml path
         case mstat of
           Nothing -> do
             td ""
             td ""
           Just stat -> do
             td $ toHtml $ pluralize (fstatAdditions stat) "addition"
             td $ toHtml $ pluralize (fstatDeletions stat) "deletion"
  -- Show individual file diffs
  let commit = commitObj cmtObj
  forM_ (zip diffs nrs) $ \(diff, nr) -> do
    let path = dpathName . diffPath $ diff
    div ! id (toValue $ "diff-" ++ show nr) $ do
      div ! class_ "diff-title curved" $ do
        span $ toHtml path
        span ! class_ "diff-show-file" $
          let shaS = show (cmtPtr commit)
          in if (dpathChanges . diffPath $ diff) == Just DeletedFile
               then ""
               else a ! class_ "sha"
                      ! href (toValue $ repo2url repo ++ "/blob/"
                                        ++ shaS ++ "/" ++ path) $
                                          toHtml $ "View file @" ++ take 6 shaS
      div ! class_ "diff-content" $ pre $
          let diffFile = B64.decodeLenient $ diffContent diff
          in fromMaybe (rawDiff diffFile) $ diffToHtml diffFile

  where getFileStat stats path = 
          -- fail is datastructure mismatch
          List.find ((==path) . fstatPath) $ statFiles stats
        rawDiff file = 
          let ls = lines $ S8.unpack file
          in toHtml . unlines . safeTail $ ls

diffToHtml :: S8.ByteString -> Maybe Html
diffToHtml file = do
  ls <- parseDiff file
  return $ 
    table ! class_ "table-striped table-bordered" $ do
      colgroup $ do
        col
        col
        col ! class_ "col-content"
      forM_ ls $ \l -> tr $ do
        let ltype = dlineType l 
            td' x = td ! class_ "diff-lineno" $ x
        case ltype of
          (Remove x) -> td' (toHtml $ show x) >> td' " "
          (Insert x) -> td' " " >> td' (toHtml $ show x)
          (Common x) -> td' (toHtml $ show x) >> td' (toHtml $ show x)
          _          -> td' "..." >> td' "..."
        td $ div ! class_ (typeToClass ltype) $ do
                 toHtml . S8.unpack $ dlineCont l
 where typeToClass t = case t of
         Remove _ -> "diff-remove"
         Insert _ -> "diff-insert"
         Common _ -> "diff-common"
         Hunk     -> "diff-hunk"

-- | Show a blob
viewBlob :: Repo
         -> SHA1
         -> GitBlob
         -> [String]
         -> Html
viewBlob repo sha blob dirs = do
  let title = List.head dirs -- branch name
      -- (name, path) pairs for building breadcrumb links
      links' = foldl (\paths d -> paths ++ [(d, (snd . List.last) paths ++ "/" ++ d)])
                     [(title, title)] (safeTail dirs)
      (objName, objPath)  = last links'
      links = init links'
  ul ! class_"breadcrumb" $ do
    li $ do a ! href (toValue $ repo2url repo) $ span ! class_ "icon-home" $ " "
            span ! class_ "divider" $ "/"
    forM_ links $ \(n,lnk) -> 
        li $ do a ! href (toValue $ repo2url repo ++ "/tree/" ++ lnk) $ toHtml n
                span ! class_ "divider" $ "/"
    li $ a ! href (toValue $ repo2url repo ++ "/blob/" ++ objPath) $
         toHtml objName
    when (takeExtension objName `elem` [".c", ".hs"]) $
      a ! href (toValue $ repo2url repo ++ "/lint/" ++ List.intercalate "/" dirs)
        ! class_ "lint" $ do
          span ! class_ "icon-question-sign white" $ "" 
          " Lint"
  div $ pre ! class_ "prettyprint linenums" $
        toHtml . S8.unpack . B64.decodeLenient $ blobContent blob

-- | Show a filtered blob
viewFilteredBlob :: Repo
                 -> SHA1
                 -> GitBlob
                 -> String
                 -> S8.ByteString
                 -> [String]
                 -> Html
viewFilteredBlob repo sha blob filterTitle fblob dirs = do
  let title = List.head dirs -- branch name
      -- (name, path) pairs for building breadcrumb links
      links' = foldl (\paths d -> paths ++ [(d, (snd . List.last) paths ++ "/" ++ d)])
                     [(title, title)] (safeTail dirs)
      (objName, objPath)  = last links'
      links = init links'
  ul ! class_"breadcrumb" $ do
    li $ do a ! href (toValue $ repo2url repo) $ span ! class_ "icon-home" $ " "
            span ! class_ "divider" $ "/"
    forM_ links $ \(n,lnk) -> 
        li $ do a ! href (toValue $ repo2url repo ++ "/tree/" ++ lnk) $ toHtml n
                span ! class_ "divider" $ "/"
    li $ a ! href (toValue $ repo2url repo ++ "/blob/" ++ objPath) $
         toHtml objName
  div $ pre ! class_ "prettyprint linenums" $
        toHtml . S8.unpack . B64.decodeLenient $ blobContent blob
  div $ do h3 $ toHtml filterTitle
           pre $ toHtml . S8.unpack $ fblob


-- | Get the mime type based on extension
getMimeType :: FilePath -> String
getMimeType path =
  let ext = takeExtension path
  in if null ext
      then "application/octet-stream" -- unknown
      else S8.unpack $ systemMimeMap (tail ext)

-- | Create: entry-icon <a href="entry-type/branch/entry-path">entry-path</a>
htmlTreeEntry :: Repo -> GitTreeEntry -> String -> Html
htmlTreeEntry repo ent branch = do
  span ! class_ (entIcon ent) $ " "
  void " "
  a ! href (toValue $ repo2url repo ++ "/" ++ entObjType ent
                                    ++ "/" ++ branch
                                    ++ "/" ++ entPath ent) $
    toHtml $ entPath ent

-- | Getpath prefix for a tree entry
entObjType :: GitTreeEntry -> String
entObjType ent = case entType ent of
  GtTree   -> "tree"
  GtBlob   -> "blob"
  GtTag    -> "tag"
  GtCommit -> "commit"

-- | Convert tree entry type to icon
entIcon :: GitTreeEntry -> AttributeValue
entIcon ent = case entType ent of
  GtTree   -> "icon-folder-open"
  GtBlob   -> "icon-file"
  GtTag    -> "icon-tag"
  GtCommit -> "icon-flag"

-- | Generate alert-box with commit message and stats
htmlCommitBox :: Repo -> CommitObj -> Html
htmlCommitBox repo cmtObj = do
  let commit = commitObj cmtObj
      stats  = commitStats cmtObj
  div ! class_ "alert fade in alert-commit" $ do
    a ! class_ "close" !
      dataAttribute "dismiss" "alert"  $ preEscapedString "&times;"
    div ! class_ "row-fluid" $ do
      let author = cmtAuthor commit
          message = lines . S8.unpack . cmtMessage $ commit
      div ! class_ "span8" $ do
        h4 ! class_ "alert-heading" $ do
          span ! class_ "icon-comment" $ " "
          toHtml $ " " ++ safeHead "" message
        blockquote $ do
          div ! id "commit-message" $
            forM_ (rmFrontWS . safeTail $  message) $ \l -> toHtml l >> br
          a ! id "commit-message-show" ! href "#" $ 
            span ! class_ "icon-chevron-down" $ ""
          void " "
          a ! id "commit-message-hide" ! href "#" $ 
            span ! class_ "icon-chevron-up" $ ""
          p . em . small . toHtml $ "Authored " ++ (S8.unpack $ authDate author)
                                    ++ " by " ++ (S8.unpack $ authName author)
                                    ++ " <"  ++ (S8.unpack $ authEmail author)
                                    ++ "> "
      div ! class_ "span4 commit-info" $ do
        div $ showSha True (cmtPtr commit)
        p $ do
          toHtml $ pluralize (length . statFiles $ stats) "file"
                 ++ " changed, with "
                 ++ pluralize (statAdditions stats) "addition"
                 ++ " and "
                 ++ pluralize (statDeletions stats) "deletion"
                 ++ "."
          br
          let parents = cmtParents commit
          toHtml ("parent" ++ if length parents /= 1
                                  then "s: " else ": " :: String)
          forM_ parents $ \sha -> showSha False sha >> " "
      where rmFrontWS = dropWhile (all isSpace)
            showSha isCommit sha = 
              let shaS = show sha
                  classes :: String
                  classes = "sha" ++ if isCommit then " label" else ""
              in a ! class_ (toValue classes)
                   ! href (toValue $ repo2url repo ++ "/commit/" ++ shaS) 
                   $ toHtml (take 6 shaS)
--
-- Misc
--

-- | Print a number and pluralize suffix
pluralize :: Int -> String -> String
pluralize n s = show n ++ " " ++ (if n == 1 then s else s++"s")

repo2url :: Repo -> String
repo2url r = "/" ++ repoOwner r ++ "/" ++ repoName r

safeHead :: a -> [a] -> a
safeHead def ls = fromMaybe def $ listToMaybe ls

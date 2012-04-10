{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Repo ( viewTreeOrCommit
                  , viewBlob
                  ) where

import Prelude hiding (div, span, id)
import Control.Monad

import qualified Data.List as List
import qualified Data.ByteString.Char8 as S8

import Text.Blaze.Html5 hiding (title, style)
import Text.Blaze.Html5.Attributes hiding (label, form, span, title)

import Gitstar.Repo

import Utils


-- | Show a commit or tree.
viewTreeOrCommit :: Repo
                 -> SHA1
                 -> Maybe GitCommit
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
  ul ! class_"breadcrumb" $ 
    forM_ links $ \(n,lnk) -> 
        li $ do a ! href (toValue $ repo2url repo ++ "/tree/" ++ lnk) $ toHtml n
                span ! class_ "divider" $ "/"
  case mcommit of
    Nothing -> return ()
    Just commit -> 
      div ! class_ "alert fade in alert-commit" $ do
        a ! class_ "close" !
          dataAttribute "dismiss" "alert"  $ preEscapedString "&times;"
        let author = cmtAuthor commit
        h4 ! class_ "alert-heading" $ do
          span ! class_ "icon-comment" $ " "
          toHtml $ "  " ++ (S8.unpack $ authName author)
                ++ " <" ++ (S8.unpack $ authEmail author) ++ "> "
        blockquote $ do
          p . toHtml . S8.unpack $ cmtMessage commit
          p . em . small . toHtml $ "Authored " ++ (S8.unpack $ authDate author)
  table ! class_ "table table-bordered table-condensed" $ do
    thead $ tr $ th "name"
    tbody $ forM_ tree $ \ent -> tr $ th $ htmlTreeEntry repo ent path

-- | Show a blob
viewBlob :: Repo
         -> SHA1
         -> GitBlob
         -> [String]
         -> Html
viewBlob repo _  blob dirs = do
  let title = List.head dirs -- branch name
      -- (name, path) pairs for building breadcrumb links
      links' = foldl (\paths d -> paths ++ [(d, (snd . List.last) paths ++ "/" ++ d)])
                     [(title, title)] (safeTail dirs)
      (objName, objPath)  = last links'
      links = init links'
  ul ! class_"breadcrumb" $ do
    forM_ links $ \(n,lnk) -> 
        li $ do a ! href (toValue $ repo2url repo ++ "/tree/" ++ lnk) $ toHtml n
                span ! class_ "divider" $ "/"
    li $ a ! href (toValue $ repo2url repo ++ "/blob/" ++ objPath) $
         toHtml objName
  div $ pre ! class_ "prettyprint linenums" $
        toHtml $ S8.unpack $ blobContent blob
              

-- | Create: entry-icon <a href="entry-type/branch/entry-path">entry-path</a>
htmlTreeEntry :: Repo -> GitTreeEntry -> String -> Html
htmlTreeEntry repo ent branch = do
  span ! class_ (entIcon ent) $ " "
  " "
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
  GtCommit -> "icon-map-marker"

--
-- Misc
--

repo2url :: Repo -> String
repo2url r = "/" ++ repoOwner r ++ "/" ++ repoName r

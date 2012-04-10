{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Welcome (welcomeView) where

import Prelude hiding (div, span)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (id, label, form, span)

welcomeView :: Html
welcomeView = do
  div ! class_ "hero-unit" $ do
    h1 $ "Gitstar Viewer"
    p $ toHtml msg
    p $ a ! href "/scs/gitstar-viewer" ! class_ "btn btn-large btn-primary" $
          "Browse this project!"
   where msg :: String
         msg = "A simple app for browsing gitstar git repositories." ++
               " To view project repos point your browser to" ++
               " /user/project/"

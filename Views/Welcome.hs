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
    p $ toHtml $ ( "It's not the M or the C...." :: String)

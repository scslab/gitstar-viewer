{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Welcome ( welcome ) where

import Layouts
import Views.Welcome

import LIO.DCLabel

import Data.IterIO.Http.Support

welcome :: Action t b DC ()
welcome = renderHtml welcomeView

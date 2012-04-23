{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
module Viewer where

import Data.Monoid
import Hails.App
import Data.IterIO.Http.Support
import Control.Monad (void)

import Controllers

server :: AppReqHandler
server = runAction $ do
  req <- getHttpReq
  prms0 <- params
  body <- getBody >>= (liftLIO . unlabel)
  prms1 <- parseParams' req body
  void . setParams $ prms1 ++ prms0
  runActionRoute $ mconcat 
    [ routeTop $ routeAction welcome
    , routeMethod "GET" $
        routePattern "/:user_name/:project_name/lint/:id" $
                     routeAction showLint
    , routeMethod "GET" $
        routePattern "/:user_name/:project_name/tree/:id" $
                     routeAction showTreeOrCommit
    , routeMethod "GET" $
        routePattern "/:user_name/:project_name/blob/:id" $
                     routeAction showBlob
    , routeMethod "GET" $
        routePattern "/:user_name/:project_name/commit/:id" $
                     routeAction showCommit
    , routeMethod "GET" $
        routePattern "/:user_name/:project_name" $
                     routeAction showBranches
    ]

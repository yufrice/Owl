{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Delete where

import System.Directory
import Import

getDeleteR :: ProductId -> Handler Html
getDeleteR productId = do
  product <- runDB $ get productId
  case product of
    Just product -> do
      runDB $ delete productId
      liftIO $ removeFile $ (</>) "static/images" $ unpack $ productImage product
      redirect AdminR
      
    _ -> defaultLayout [whamlet| error |]
  

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE RankNTypes #-}

module Api
  ( module Api.Data
  , module Api
  ) where

import  Import
import  Api.Data
import  Yesod
import Database.MongoDB.Query

instance (Yesod master
         ,YesodPersistBackend master ~ MongoContext
         ,YesodPersist master
         ) =>
         YesodSubDispatch ApiSub master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesApiSub)

data Search = Search {
  desc :: Text
}

instance FromJSON Search where
  parseJSON (Object v) = Search
      <$> v .: "desc"


getSearchR :: Yesod master => SubHandlerFor ApiSub master Value
getSearchR =  returnJson $ object []

postSearchR :: forall master. (Yesod master, YesodPersistBackend master ~ MongoContext, YesodPersist master) => SubHandlerFor ApiSub master Value
postSearchR = do
  (Search search) <- liftHandler requireCheckJsonBody :: SubHandlerFor ApiSub master Search
  items <- liftHandler $ runDB $ selectList [ItemName ==. search ] []
  returnJson items
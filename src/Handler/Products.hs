{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Products where

import Import

getProductsR :: ProductId -> Handler Html
getProductsR productId = defaultLayout [whamlet| #{show productId}|]

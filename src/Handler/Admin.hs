{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import qualified Yesod.Form.Bootstrap4 as Bs4

data FileForm = FileForm
    { fileName :: Text
    , fileVec :: Text
    , fileImage :: FileInfo 
    }

getAdminR :: Handler Html
getAdminR = do
    products <- runDB $ selectList [] [Asc ProductId]
    (formWidget, enctype) <- generateFormPost fileForm
    defaultLayout $(widgetFile "admin")

fileForm :: Form FileForm
fileForm = Bs4.renderBootstrap4 Bs4.BootstrapBasicForm $ FileForm
    <$> areq textField (Bs4.bfs (""::Text)) Nothing
    <*> areq textField (Bs4.bfs (""::Text)) Nothing
    <*> fileAFormReq ""
    <* Bs4.bootstrapSubmit ("Submit" :: Bs4.BootstrapSubmit Text)
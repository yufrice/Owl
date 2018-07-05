{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import qualified Yesod.Form.Bootstrap4 as Bs4
import Text.Julius (RawJS (..))

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (searchWidget, enctype) <- generateFormPost searchForm
    defaultLayout $ do
        setTitle "home"
        [whamlet|
            <div .container-fluid>
                <div .row>
                    ^{navber searchWidget enctype}
                    <div .col-md-8>
                        <div .container>
                            ^{display}
        |]

postHomeR :: Handler Html
postHomeR = do
    ((result, searchWidget), enctype) <- runFormPost searchForm
    case result of
        FormSuccess result -> do
            products <- runDB $ selectList [ProductName ==. result] [Asc ProductId]
            defaultLayout $ do
                setTitle "home"
                [whamlet|
                    <div .container-fluid>
                        <div .row>
                            ^{navber searchWidget enctype}
                            <div .col-md-8>
                                <div .container>
                                    ^{display}
                                    ^{resultView products}
                |]
        _-> defaultLayout $(widgetFile "homepage")

navber :: Widget -> Enctype -> Widget
navber searchWidget enctype = $(widgetFile "navber")

display :: Widget
display = toWidget [whamlet|
        <div .jumbotron>
            <h1 .display-4>
                text
            <p>
                text text
    |]

resultView :: [Entity Product] -> Widget
resultView products = $(widgetFile "result")

searchForm:: Form Text
searchForm = Bs4.renderBootstrap4 Bs4.BootstrapBasicForm
        $ areq textField (Bs4.bfs (""::Text)) Nothing
        <* Bs4.bootstrapSubmit ("Search" :: Bs4.BootstrapSubmit Text)
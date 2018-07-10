{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import qualified Yesod.Form.Bootstrap4 as Bs4

getHomeR :: Handler Html
getHomeR = do
    (searchWidget, enctype) <- generateFormPost searchForm
    defaultLayout $ do
        setTitle "home"
        [whamlet|
            <div .container-fluid>
                <div .row>
                    ^{navber searchWidget enctype}
                    <div .col-lg-8>
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
                            <div .col-lg-8>
                                ^{display}
                                ^{resultView products}
                |]
        _-> defaultLayout $ do
            setTitle "home"
            [whamlet|
                <div .container-fluid>
                    <div .row>
                        ^{navber searchWidget enctype}
                        <div .col-lg-8>
                            ^{display}
            |]

navber :: Widget -> Enctype -> Widget
navber searchWidget enctype = $(widgetFile "navber")

display :: Widget
display = toWidget [whamlet|
        <div .jumbotron.mt-5>
            <h1 .display-4>
                text
            <p>
                text text
    |]

resultView :: [Entity Product] -> Widget
resultView products = $(widgetFile "result")

searchForm :: Form Text
searchForm = Bs4.renderBootstrap4 Bs4.BootstrapBasicForm
        $ areq textField (Bs4.bfs (""::Text)) Nothing
        <* Bs4.bootstrapSubmit ("Search" :: Bs4.BootstrapSubmit Text)
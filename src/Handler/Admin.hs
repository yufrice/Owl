{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Crypto.Hash.Conduit (sinkHash)
import Text.Regex (mkRegex, subRegex)
import qualified Yesod.Form.Bootstrap4 as Bs4

data FileForm = FileForm
    { name :: Text
    , fileDesc :: Text
    , fileImage :: FileInfo 
    }

getAdminR :: Handler Html
getAdminR = do
    products <- runDB $ selectList [] [Asc ProductId]
    (formWidget, enctype) <- generateFormPost fileForm
    defaultLayout $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
    ((result, _), _) <- runFormPost fileForm
    case result of
        FormSuccess (FileForm name desc fileInfo) -> do
            uri <- writeToServer fileInfo
            runDB $ insert $ Product {
                productName = name
                , productImage = pack uri
                , productVector = desc
            }
            defaultLayout [whamlet| fileInfo|]
        _ ->
            redirect AdminR

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    hash <- hashFromFile file
    uri <- return $ imageFilePath $ show hash <.> typeFromName file
    liftIO $ fileMove file uri
    return $ show hash <.> typeFromName file

typeFromName :: FileInfo -> String
typeFromName file = subRegex (mkRegex "\"") 
    (subRegex (mkRegex "^[^.]*.") (show $ fileName file) "") ""

imageFilePath :: String -> FilePath
imageFilePath = (("static" </> "images") </>)

hashFromFile :: MonadUnliftIO m => FileInfo -> m (Digest SHA256)
hashFromFile = liftIO . runConduitRes . (.| sinkHash) . fileSource


fileForm :: Form FileForm
fileForm = Bs4.renderBootstrap4 Bs4.BootstrapBasicForm $ FileForm
    <$> areq textField (Bs4.bfs (""::Text)) Nothing
    <*> areq textField (Bs4.bfs (""::Text)) Nothing
    <*> areq fileField
        (FieldSettings "" Nothing Nothing Nothing [("accept", "image/*")]) Nothing
    <* Bs4.bootstrapSubmit ("Submit" :: Bs4.BootstrapSubmit Text)
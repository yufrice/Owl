{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash.Conduit as ConduitH
import Database.Persist.MongoDB (unMongoKey)
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
            case uri of
                Just uri -> do
                    $(logInfo) "write"
                    runDB $ insert $ Product {
                        productName = name 
                        , productImage = pack uri
                        , productVector = desc
                    }
                    redirect AdminR
                Nothing -> invalidArgs ["Conflict Image"]
        _ -> badMethod

writeToServer :: FileInfo -> Handler (Maybe FilePath)
writeToServer file = do
    hash <- hashFromFile file
    uri <- return $ show hash <.> typeFromName file
    products <- runDB $ selectList [ProductImage ==. pack uri] []
    $(logInfo) $ pack uri
    case products of
        [] -> do
            liftIO $ fileMove file $ imageFilePath uri
            return <$> Just $ show hash <.> typeFromName file
        _ -> return $ Nothing

typeFromName :: FileInfo -> String
typeFromName file = subRegex (mkRegex "\"") 
    (subRegex (mkRegex "^[^.]*.") (show $ fileName file) "") ""

imageFilePath :: String -> FilePath
imageFilePath = (("static" </> "images") </>)

hashFromFile :: MonadUnliftIO m => FileInfo -> m (Digest SHA256)
hashFromFile = liftIO . runConduitRes . (.| ConduitH.sinkHash) . fileSource

deleteModalId :: ProductId -> Text
deleteModalId = pack .(++) "#deleteModal_" . show

fileForm :: Form FileForm
fileForm = Bs4.renderBootstrap4 Bs4.BootstrapBasicForm $ FileForm
    <$> areq textField (Bs4.bfs (""::Text)) Nothing
    <*> areq textField (Bs4.bfs (""::Text)) Nothing
    <*> areq fileField
        (FieldSettings "" Nothing Nothing Nothing [("accept", "image/*")]) Nothing
    <* Bs4.bootstrapSubmit ("Submit" :: Bs4.BootstrapSubmit Text)
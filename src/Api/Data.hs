{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-#LANGUAGE RankNTypes#-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Data where

import Yesod

data ApiSub = ApiSub

mkYesodSubData "ApiSub" $(parseRoutesFile "config/apiRoutes")
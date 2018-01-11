{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Lucid (Html)
import Servant.API
import Servant.HTML.Lucid (HTML)
import Test 

type Api = PersonApi :<|> HtmlApi :<|> StaticApi

type PersonApi = "person" :> Get '[JSON] Person

type HtmlApi = Get '[HTML] (Html ())

type StaticApi = Raw

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Lucid (Html)
import Servant.API
import Servant.HTML.Lucid (HTML)
import Test

type Api = PersonApi :<|> LoginApi :<|> HtmlApi :<|> StaticApi

type PersonApi = "person" :> Get '[JSON] Person

type LoginApi  = "login" :> ReqBody '[JSON] String :> Post '[JSON] Person

type HtmlApi = Get '[HTML] (Html ())

type StaticApi = Raw

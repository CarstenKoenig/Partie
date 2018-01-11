{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (application, main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Servant as S
import Servant.HTML.Lucid (HTML)
import ApiType
import Test
import Lucid (Html)
import qualified Lucid as L


main :: IO ()
main = run 8081 application


application :: Application
application = S.serve (S.Proxy :: S.Proxy Api) server


server :: S.Server Api
server = handlePerson S.:<|> serveRoot S.:<|> serveStaticFiles


serveStaticFiles = S.serveDirectoryWebApp "static"


handlePerson :: S.Handler Person
handlePerson = return $ Person "Carsten" 38


serveRoot :: S.Handler (Html ())
serveRoot = liftIO $ do
  index <- BS.readFile "html/index.html"
  return $ L.toHtmlRaw index

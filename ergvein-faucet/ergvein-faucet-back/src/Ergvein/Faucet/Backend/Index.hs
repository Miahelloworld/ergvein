module Ergvein.Faucet.Backend.Index(
    HttpAPI
  , httpServer
  ) where

import Data.Foldable (traverse_)
import Data.Monoid
import Data.Text (Text)
import Network.Socket (SockAddr(..))
import Prelude hiding (head, id, div)
import Servant.API
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Ergvein.Faucet.API
import Ergvein.Faucet.Backend.Monad

import qualified Data.Text as T

type IndexEndpoint = RemoteHost :> UserAgentHeader :> Get '[HTML] IndexPage

type HttpAPI = IndexEndpoint

data IndexPage = IndexPage {
  indexPageBlobUrl :: Text
}

httpServer :: ServerT HttpAPI ServerM
httpServer = indexEndpoint

indexEndpoint :: SockAddr -> Maybe UserAgent -> ServerM IndexPage
indexEndpoint _ _ = do
  blobHash <- getMainJsHash
  pure IndexPage {
      indexPageBlobUrl = "/main_" <> toUrlPiece blobHash <> ".js"
    }

instance ToMarkup IndexPage where
  toMarkup IndexPage{..} = docTypeHtml $ do
    head $ do
      meta ! charset "utf-8"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1, shrink-to-fit=no"
      title "Ergvein testnet faucet"
      makeIcons
      makeStyles
    body ! class_ "mybody" $ do
      div ! id "loader-wrapper" $ div ! id "loader" $ do
        div ! class_ "loadlogo" $ pure ()
        logoSvg
      div ! id "content" $ do
        makeScripts
    where
      makeIcons = do
        let mkAppleTouchIcon sz = link ! rel "apple-touch-icon" ! sizes sz ! href ("/static/favicon/apple-icon-" <> sz <> ".png")
        traverse_ mkAppleTouchIcon ["57x57", "60x60", "72x72", "76x76", "114x114", "120x120", "144x144", "152x152", "180x180"]
        link ! rel "icon" ! type_ "image/png" ! sizes "192x192" ! href "/static/favicon/android-icon-192x192.png"
        let mkFavicon sz = link ! rel "icon" ! type_ "image/png" ! sizes sz ! href ("/static/favicon/favicon-" <> sz <> ".png")
        traverse_ mkFavicon ["32x32", "96x96", "16x16"]
        link ! rel "manifest" ! href "/static/favicon/manifest.json"
        meta ! name "msapplication-TileColor" ! content "#ffffff"
        meta ! name "msapplication-TileImage" ! content "/static/favicon/ms-icon-144x144.png"
        meta ! name "theme-color" ! content "#ffffff"

      makeStyles = do
        let mkStyle path = link ! rel "stylesheet" ! href path
        traverse_ mkStyle [

          ]

      makeScripts = do
        let mkScript v = script ! src v $ pure ()
        traverse_ mkScript [

          ]
        script ! src (toValue indexPageBlobUrl) ! defer "" $ pure ()

      logoSvg = preEscapedToHtml $ T.unlines [
          "<svg version=\"1.1\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" x=\"0px\" y=\"0px\" viewBox=\"0 0 500 500\""
        , "style=\"enable-background:new 0 0 500 500;\" xml:space=\"preserve\" class=\"loaderlogosvg\">"
        , "</svg>" ]

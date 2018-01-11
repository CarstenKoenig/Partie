module Main where

import Prelude hiding (div)

import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Test (Person(..))
import Text.Smolder.HTML (a, br, button, div, nav, form, h1, input, li, span, ul)
import Text.Smolder.HTML.Attributes (className, href, id, style, name, type', placeholder)
import Text.Smolder.Markup (text, (#!), (!), attribute)

data Event = Increment | Decrement | RequestPerson | ReceivePerson (Either String Person)

type State = Int

carsten :: Person
carsten = Person { name: "Carsten", age: 38 }

-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event (ajax :: AJAX , console :: CONSOLE | fx)
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }
foldp RequestPerson n =   
  { state: n
  , effects: [ do
      res <- attempt $ get "/person"
      let decode r = decodeJson r.response :: Either String Person
      let person = either (Left <<< show) decode res
      pure $ Just $ ReceivePerson person
    ]
  }
foldp (ReceivePerson (Left err)) n = 
  { state: n
  , effects: [ do
      liftEff (log err) 
      pure Nothing
    ] 
  }
foldp (ReceivePerson (Right (Person p))) n = 
  { state: n
  , effects: [ do
      liftEff $ log p.name
      pure Nothing
    ] 
  }

-- | Return markup from the state
view :: State -> HTML Event
view count = do
  nav ! className "navbar navbar-inverse navbar-fixed-top" $ do
    div ! className "container" $ do
      div ! className "navbar-header" $ do
        button ! className "navbar-toggle collapsed" ! attribute "data-toggle" "collapse" ! attribute "data-target" "#navbar" $ do
          span ! className "icon-bar" $ text ""
        a ! className "navbar-brand" ! href "#" $ text "Partie"
      div ! id "navbar" ! className "navbar-collapse collapse" $ do
        form ! className "navbar-form navbar-right" $ do
          div ! className "form-group" $ do
            input ! name "username" ! id "username" ! type' "text" ! placeholder "User" ! className "form-control"
          div ! className "form-group" $ do
            input ! name "password" ! id "password" ! type' "password" ! placeholder "password" ! className "form-control"
          button ! type' "submit" ! className "btn btn-success" $ text "Login"

  div ! className "jumbotron" $ do
    div ! className "container" $ do
      h1 $ text "Hi Bootstrap"

  div ! className "container" $ do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"
    button #! onClick (const RequestPerson) $ text "Load"


-- | Start and render the app
main :: ∀ fx. Eff (ajax :: AJAX, console :: CONSOLE | CoreEffects fx) Unit
main = do
  app <- start
    { initialState: 0
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input

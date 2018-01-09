module Main where

import Prelude hiding (div)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Network.HTTP.Affjax (AJAX, get)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Test (Person(..))
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

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
view count =
  div do
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
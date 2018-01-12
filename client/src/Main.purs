module Main where

import Prelude hiding (div,id)

import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get, post)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick, onSubmit, onInput, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Test (Person(..))
import Text.Smolder.HTML (a, button, div, nav, form, h1, input, span)
import Text.Smolder.HTML.Attributes (className, href, id, name, type', placeholder)
import Text.Smolder.Markup (text, (#!), (!), attribute)


data Msg 
  = RequestPerson 
  | ReceivePerson (Either String Person)
  | LoginUser
  | UserNameInput String

data State = State
  { userNameInput :: String
  }

derive instance genericState :: Generic State _

instance showState :: Show State where
  show = genericShow

carsten :: Person
carsten = Person { name: "Carsten", age: 38 }

-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Msg -> State -> EffModel State Msg (ajax :: AJAX , console :: CONSOLE | fx)
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
foldp LoginUser n@(State s) = 
  { state: n
  , effects: [  do
      res <- attempt $ post "/login" s.userNameInput
      let decode r = decodeJson r.response :: Either String Person
      let person = either (Left <<< show) decode res
      pure $ Just $ ReceivePerson person
  ] 
  }
foldp (UserNameInput inp) s@(State n) = 
  { state: s
  , effects: [] 
  }
  

-- | Return markup from the state
view :: State -> HTML Msg
view state = do
  viewNavbar state

  div ! className "jumbotron" $ do
    div ! className "container" $ do
      h1 $ text "Hi Bootstrap"

  div ! className "container" $ do
    span $ text (show state)
    button #! onClick (const RequestPerson) $ text "Load"


viewNavbar :: State -> HTML Msg
viewNavbar state = do
  nav ! className "navbar navbar-inverse navbar-fixed-top" $ do
    div ! className "container" $ do
      div ! className "navbar-header" $ do
        button ! className "navbar-toggle collapsed" ! attribute "data-toggle" "collapse" ! attribute "data-target" "#navbar" $ do
          span ! className "icon-bar" $ text ""
        a ! className "navbar-brand" ! href "#" $ text "Partie"
      div ! id "navbar" ! className "navbar-collapse collapse" $ do
        viewLogin state

viewLogin :: State -> HTML Msg            
viewLogin state = do
  form ! className "navbar-form navbar-right" #! onSubmit (const LoginUser) $ do
    div ! className "form-group" $ do
      input ! name "username" ! id "username" ! type' "text" ! placeholder "User" ! className "form-control" #! onInput (targetValue >>> UserNameInput)
    button ! type' "submit" ! className "btn btn-success" $ text "Login"

-- | Start and render the app
main :: ∀ fx. Eff (ajax :: AJAX, console :: CONSOLE | CoreEffects fx) Unit
main = do
  app <- start
    { initialState: State { userNameInput : "" }
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input




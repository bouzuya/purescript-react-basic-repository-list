module Component.App
  ( app
  ) where

import Bouzuya.HTTP.Client as HttpClient
import Bouzuya.HTTP.Method as Method
import Component.AppStyle as Style
import Data.Either (either)
import Data.Maybe (fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Options ((:=))
import Effect (Effect)
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Aff as Aff
import Prelude (Unit, bind, map, max, pure, show, (+), (-), (<#>), (<<<), (<>))
import React.Basic (Component, JSX, Self, StateUpdate(..), capture_, createComponent, make, send, sendAsync)
import React.Basic.DOM as H
import Simple.JSON (E)
import Simple.JSON as SimpleJSON

type Props =
  {}

type State =
  { page :: Int
  , repos :: Array Repo
  }

data Action
  = FetchFailure Error
  | FetchRepos
  | NextPage
  | PrevPage
  | UpdateRepos (Array Repo)

type Repo =
  { full_name :: String
  , language :: Nullable String
  , stargazers_count :: Int
  , updated_at :: String
  }

fetchRepos :: Int -> Aff (Array Repo)
fetchRepos page = do
  { body } <- HttpClient.fetch
    ( HttpClient.method := Method.GET
    <> HttpClient.url := ("https://api.github.com/users/bouzuya/repos?type=owner&sort=pushed&direction=desc&per_page=100&page=" <> show page)
    )
  b <- maybe (throwError (error "body is nothing")) pure body
  either
    (throwError <<< error <<< show)
    pure
    (SimpleJSON.readJSON b :: E (Array Repo))

fetchRepos' :: Int -> Aff Action
fetchRepos' page =
  map (either FetchFailure UpdateRepos) (Aff.try (fetchRepos page))

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { didMount, initialState, render, update } {}

didMount :: Self Props State Action -> Effect Unit
didMount self = send self FetchRepos

initialState :: State
initialState =
  { page: 1
  , repos: []
  }

renderRepoRow :: Repo -> JSX
renderRepoRow repo =
  H.tr
  { className: "repo"
  , children:
    [ H.td_
      [ H.a
        { href: "https://github.com/" <> repo.full_name <> "/"
        , children:
          [ H.text repo.full_name ]
        }
      ]
    , H.td_
      [ H.text (fromMaybe "" (toMaybe repo.language)) ]
    , H.td_
      [ H.text (show repo.stargazers_count) ]
    , H.td_
      [ H.text repo.updated_at ]
    ]
  }

renderPager :: Self Props State Action -> JSX
renderPager self =
  H.div
  { className: Style.pager
  , children:
    [ H.div
      { children: [ H.text "<" ]
      , className: Style.prev
      , onClick: capture_ self PrevPage
      }
    , H.div
      { children: [ H.text (show self.state.page) ]
      , className: Style.pageNumber
      }
    , H.div
      { children: [ H.text ">" ]
      , className: Style.next
      , onClick: capture_ self NextPage
      }
    ]
  }

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: "app"
  , children:
    [ H.div
      { className: "header"
      , children:
        [ H.h1_
          [ H.text "Repository List" ]
        ]
      }
    , H.div
      { className: "body"
      , children:
        [ renderPager self
        , H.table_ (self.state.repos <#> (\repo -> renderRepoRow repo))
        ]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self (FetchFailure _) =
  Update self.state { repos = [] }
update self FetchRepos =
  UpdateAndSideEffects
    (self.state { repos = [] })
    (\self' -> sendAsync self' (fetchRepos' self'.state.page))
update self NextPage =
  UpdateAndSideEffects
    (self.state { page = self.state.page + 1 })
    (\self' -> send self' FetchRepos)
update self PrevPage =
  UpdateAndSideEffects
    (self.state { page = max 1 (self.state.page - 1) })
    (\self' -> send self' FetchRepos)
update self (UpdateRepos repos) =
  Update self.state { repos = repos }

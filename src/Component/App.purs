module Component.App
  ( app
  ) where

import Bouzuya.HTTP.Client as HttpClient
import Bouzuya.HTTP.Method as Method
import Component.AppStyle as Style
import Data.Array as Array
import Data.Either (either)
import Data.Enum (enumFromTo)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Options ((:=))
import Data.RepoOrder (RepoOrder(..))
import Data.RepoOrder as RepoOrder
import Effect (Effect)
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Aff as Aff
import Prelude (Unit, bind, bottom, map, max, pure, show, top, (+), (-), (<#>), (<<<), (<>), (==))
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, capture_, createComponent, make, send, sendAsync)
import React.Basic.DOM as H
import React.Basic.DOM.Events (targetValue)
import Simple.JSON (E)
import Simple.JSON as SimpleJSON

type Props =
  {}

type State =
  { loading :: Boolean
  , order :: RepoOrder
  , page :: Int
  , repos :: Array Repo
  }

data Action
  = FetchFailure Error
  | FetchRepos
  | FetchSuccess (Array Repo)
  | NextPage
  | PrevPage
  | SelectRepoOrder String
  | UpdateRepos (Array Repo)

type Repo =
  { full_name :: String
  , language :: Nullable String
  , stargazers_count :: Int
  , updated_at :: String
  }

fetchRepos :: RepoOrder -> Int -> Aff (Array Repo)
fetchRepos order page = do
  let
    baseUrl = "https://api.github.com/users/bouzuya/repos"
    query =
      Array.intercalate
        "&"
        [ "type=owner"
        , "sort=" <> show order
        , "direction=" <>
            case order of
              RepoOrder.Created -> "desc"
              RepoOrder.Pushed -> "desc"
              RepoOrder.FullName -> "asc"
              RepoOrder.Updated -> "desc"
        , "per_page=100"
        , "page=" <> show page
        ]
    url = baseUrl <> "?" <> query
  { body } <- HttpClient.fetch
    ( HttpClient.method := Method.GET
    <> HttpClient.url := url
    )
  b <- maybe (throwError (error "body is nothing")) pure body
  either
    (throwError <<< error <<< show)
    pure
    (SimpleJSON.readJSON b :: E (Array Repo))

fetchRepos' :: RepoOrder -> Int -> Aff Action
fetchRepos' order page =
  map (either FetchFailure FetchSuccess) (Aff.try (fetchRepos order page))

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { didMount, initialState, render, update } {}

didMount :: Self Props State Action -> Effect Unit
didMount self = send self FetchRepos

initialState :: State
initialState =
  { loading: true
  , order: Pushed
  , page: 1
  , repos: []
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

renderTable :: Self Props State Action -> JSX
renderTable self =
  H.table
  { className: Style.table
  , children:
    ( [ H.tr_
        [ H.th { className: Style.th, children: [ H.text "full_name" ] }
        , H.th { className: Style.th, children: [ H.text "language" ] }
        , H.th { className: Style.th, children: [ H.text "stars" ] }
        , H.th { className: Style.th, children: [ H.text "updated_at" ] }
        ]
      ] <>
      (self.state.repos <#> (\repo ->
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
      )))
  }

renderLoading :: Self Props State Action -> JSX
renderLoading self =
  H.div
  { className: if self.state.loading then Style.isLoading else Style.loading
  , children: if self.state.loading then [ H.text "loading" ] else []
  }

renderOrder :: Self Props State Action -> JSX
renderOrder self =
  H.select
  { className: Style.order
  , onChange:
      capture
        self
        targetValue
        (\v -> SelectRepoOrder (fromMaybe "" v))
  , children:
    (
      (enumFromTo bottom top :: Array RepoOrder) <#>
          (\order ->
            H.option
              { selected: order == self.state.order
              , children: [ H.text (show order) ]
              })
    )
  }

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: Style.app
  , children:
    [ H.div
      { className: "header"
      , children:
        [ H.h1
          { className: Style.title
          , children: [ H.text "Repository List" ]
          }
        ]
      }
    , H.div
      { className: "body"
      , children:
        (
          [ renderOrder self
          , renderPager self
          ] <> (
          if self.state.loading
          then []
          else
            [ H.div
              { className: Style.counter
              , children:
                [ H.text ((show (Array.length self.state.repos)) <> " repoitories")
                ]
              }
            , renderTable self
            ]
          ) <>
        [ renderLoading self ]
        )
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self (FetchFailure _) =
  Update self.state { loading = false, repos = [] }
update self FetchRepos =
  UpdateAndSideEffects
    (self.state { loading = true, repos = [] })
    (\self' -> sendAsync self' (fetchRepos' self'.state.order self'.state.page))
update self (FetchSuccess repos) =
  UpdateAndSideEffects
    self.state { loading = false }
    (\self' -> send self' (UpdateRepos repos))
update self NextPage =
  UpdateAndSideEffects
    (self.state { page = self.state.page + 1 })
    (\self' -> send self' FetchRepos)
update self PrevPage =
  UpdateAndSideEffects
    (self.state { page = max 1 (self.state.page - 1) })
    (\self' -> send self' FetchRepos)
update self (SelectRepoOrder orderString) =
  case RepoOrder.fromString orderString of
    Nothing -> NoUpdate
    Just order ->
      UpdateAndSideEffects
        (self.state { order = order })
        (\self' -> send self' FetchRepos)
update self (UpdateRepos repos) =
  Update self.state { repos = repos }

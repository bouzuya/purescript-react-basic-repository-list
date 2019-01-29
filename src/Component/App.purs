module Component.App
  ( app
  ) where

import Bouzuya.HTTP.Client as HttpClient
import Bouzuya.HTTP.Method as Method
import Component.AppStyle as Style
import Data.Array as Array
import Data.Either (either)
import Data.Enum (enumFromTo)
import Data.Maybe (fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Options ((:=))
import Data.RepoOrder (RepoOrder(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Aff as Aff
import Prelude (Unit, bind, bottom, map, max, pure, show, top, (+), (-), (<#>), (<<<), (<>), (==))
import React.Basic (Component, JSX, Self, StateUpdate(..), capture_, createComponent, make, send, sendAsync)
import React.Basic.DOM as H
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
        , "direction=desc"
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
  H.table_
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
    ))

renderLoading :: Self Props State Action -> JSX
renderLoading self =
  H.div
  { className: if self.state.loading then Style.isLoading else Style.loading
  , children: if self.state.loading then [ H.text "loading" ] else []
  }

renderOrder :: Self Props State Action -> JSX
renderOrder self =
  H.select_
  (
    (enumFromTo bottom top :: Array RepoOrder) <#>
        (\order ->
          H.option
            { selected: order == self.state.order
            , children: [ H.text (show order) ]
            })
  )

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: "app"
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
          [ renderPager self
          , renderOrder self
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
update self (UpdateRepos repos) =
  Update self.state { repos = repos }

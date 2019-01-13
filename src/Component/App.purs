module Component.App
  ( app
  ) where

import Prelude

import React.Basic (Component, JSX, Self, StateUpdate(..), capture_, createComponent, make)
import React.Basic.DOM as H

type Props =
  {}

type State =
  { page :: Int
  , repos :: Array Repo
  }

data Action
  = NextPage
  | PrevPage
  | UpdateRepos (Array Repo)

type Repo =
  { full_name :: String
  , language :: String
  , stargazers_count :: Int
  , updated_at :: String
  }

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  { page: 1
  , repos:
    [ { full_name: "bouzuya/blog.bouzuya.net"
      , language: "TypeScript"
      , stargazers_count: 6
      , updated_at: "2019-01-12T14:19:54Z"
      }
    , { full_name: "bouzuya/blog.bouzuya.net"
      , language: "TypeScript"
      , stargazers_count: 6
      , updated_at: "2019-01-12T14:19:54Z"
      }
    ]
  }

renderRepo :: Repo -> JSX
renderRepo repo =
  H.div_
  [ H.a
    { href: "https://github.com/" <> repo.full_name <> "/"
    , children:
      [ H.span_
        [ H.text repo.full_name ]
      , H.span_
        [ H.text repo.language ]
      , H.span_
        [ H.text (show repo.stargazers_count) ]
      , H.span_
        [ H.text repo.updated_at ]
      ]
    }
  ]

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: "app"
  , children:
    [ H.div
      { className: "header"
      , children:
        [ H.h1_
          [ H.text "App" ]
        ]
      }
    , H.div
      { className: "body"
      , children:
        [ H.div { children: [ H.text "<" ], onClick: capture_ self PrevPage }
        , H.div_ [ H.text (show self.state.page) ]
        , H.div { children: [ H.text ">" ], onClick: capture_ self NextPage }
        , H.ul_ (self.state.repos <#> (\repo -> H.li_ [ renderRepo repo ]))
        ]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self NextPage =
  Update self.state { page = self.state.page + 1 }
update self PrevPage =
  Update self.state { page = max 1 (self.state.page - 1) }
update self (UpdateRepos repos) =
  Update self.state { repos = repos }

module Data.RepoOrder
  ( RepoOrder(..)
  ) where

import Data.Enum (class Enum)
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show)

data RepoOrder
  = Created
  | Updated
  | Pushed
  | FullName

instance boundedRepoOrder :: Bounded RepoOrder where
  bottom = Created
  top = FullName

instance enumRepoOrder :: Enum RepoOrder where
  succ Created = Just Updated
  succ Updated = Just Pushed
  succ Pushed = Just FullName
  succ FullName = Nothing
  pred Created = Nothing
  pred Updated = Just Created
  pred Pushed = Just Updated
  pred FullName = Just Pushed

derive instance eqRepoOrder :: Eq RepoOrder

derive instance ordRepoOrder :: Ord RepoOrder

instance showRepoOrder :: Show RepoOrder where
  show Created = "created"
  show Updated = "updated"
  show Pushed = "pushed"
  show FullName = "full_name"

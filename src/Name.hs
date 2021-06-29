
module Name where

import Data.Text qualified as Text
import Data.String (IsString(..))

import Pretty

data Name = Name { raw :: Text.Text, index :: Int }
  deriving stock (Eq, Ord)
  deriving Show via PP Name

instance IsString Name where
  fromString = (`Name` 0) . fromString

instance Pretty Name where
  pretty (Name raw 0)     = pretty raw
  pretty (Name raw index) = pretty raw <.> "'" <.> pretty index
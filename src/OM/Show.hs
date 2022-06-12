{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | Utilities for showing string-like things. -}
module OM.Show (
  showt,
  showj,
  ShowJ(..),
) where


import Data.Aeson (encode, ToJSON)
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE


{- | Like 'show', but for any string-like thing. -}
showt :: (Show a, IsString b) => a -> b
showt = fromString . show


{- | Show the JSON representation as any kind of string-like thing. -}
showj :: (ToJSON a, IsString b) => a -> b
showj = fromString . TL.unpack . TLE.decodeUtf8 . encode


{- | Wrapper whose 'Show' instance outputs JSON. -}
newtype ShowJ a = ShowJ a
  deriving stock (Eq, Ord)
  deriving newtype (ToJSON)
instance (ToJSON a) => Show (ShowJ a) where
  show = showj



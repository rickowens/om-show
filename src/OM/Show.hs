
{- | Utilities for showing string-like things. -}
module OM.Show (
  showt,
) where


import Data.String (IsString, fromString)


{- | Like 'show', but for any string-like thing. -}
showt :: (Show a, IsString b) => a -> b
showt = fromString . show



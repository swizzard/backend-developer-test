module Misc where

import Control.Arrow
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

-- | (arr)ow (c)ombination -- apply two functions to the same input & combine the
-- results. Sort of the converse of 'Data.Function.on'
arrc :: (c -> c' -> d) -> (b -> c) -> (b -> c') -> b -> d
arrc f g h = (uncurry f) . (g &&& h)

tToB :: T.Text -> B.ByteString
tToB = B.pack . T.unpack

tShow :: Show a => a -> T.Text
tShow = T.pack . show

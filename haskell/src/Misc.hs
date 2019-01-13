module Misc where

import Control.Arrow

-- | (arr)ow (c)ombination -- apply two functions to the same input & combine the
-- results. Sort of the converse of 'Data.Function.on'
arrc :: (c -> c' -> d) -> (b -> c) -> (b -> c') -> b -> d
arrc f g h = (uncurry f) . (g &&& h)

module Hexpandr.Utils where

flatten :: (Functor f) => f [[p]] -> f [p]
flatten p = concat <$> p


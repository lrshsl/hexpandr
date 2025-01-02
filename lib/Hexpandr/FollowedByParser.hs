{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Hexpandr.FollowedByParser where

import Hexpandr.Parser
import Hexpandr.Utils

class FollowedBy p1 p2 r | p1 p2 -> r where
  followedBy :: p1 -> p2 -> Parser r

instance FollowedBy (Parser a) (Parser a) [a] where
  followedBy p1 p2 = (:) <$> p1 <*> fmap (:[]) p2

instance FollowedBy (Parser a) (Parser [a]) [a] where
  followedBy p1 p2 = (:) <$> p1 <*> p2

instance FollowedBy (Parser [a]) (Parser a) [a] where
  followedBy p1 p2 = (++) <$> p1 <*> fmap (:[]) p2

followedByFlat :: Parser [a] -> Parser [a] -> Parser [a]
followedByFlat p1 p2 = flatten $ p1 `followedBy` p2

-- silentFollowedBy :: Parser () -> Parser [a] -> Parser [a]
-- silentFollowedBy p1 p2 = flatten <$> p1 `followedBy` p2

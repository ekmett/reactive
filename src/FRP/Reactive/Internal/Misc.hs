----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.Misc
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Misc Reactive internal defs
----------------------------------------------------------------------

module FRP.Reactive.Internal.Misc (Action, Sink) where


-- | Convenient alias for dropping parentheses.
type Action = IO ()

-- | Value consumer
type Sink a = a -> Action

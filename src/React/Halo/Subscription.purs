module React.Halo.Subscription
  ( Emitter
  , makeEmitter
  , runEmitter
  ) where

import Prelude (Unit)

import Effect (Effect)

-- | A source that broadcasts actions to each registered receiver.
-- |
-- | Registration returns the cleanup effect for that receiver. Halo runs the
-- | cleanup when the subscription is removed or its activation scope ends.
-- | Emitters broadcast and do not provide consuming-queue or backpressure
-- | semantics.
newtype Emitter action = Emitter
  ((action -> Effect Unit) -> Effect (Effect Unit))

-- | Create an emitter from registration logic. A throwing cleanup is isolated
-- | from other scope cleanup and reported as `DeactivationError`.
makeEmitter
  :: forall action
   . ((action -> Effect Unit) -> Effect (Effect Unit))
  -> Emitter action
makeEmitter = Emitter

-- | Register a receiver and obtain its cleanup effect.
runEmitter
  :: forall action
   . Emitter action
  -> (action -> Effect Unit)
  -> Effect (Effect Unit)
runEmitter (Emitter register) = register

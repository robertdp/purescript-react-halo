module React.Halo.Subscription
  ( Emitter
  , makeEmitter
  , runEmitter
  ) where

import Prelude (Unit)

import Effect (Effect)

-- | A source that broadcasts actions to each registered receiver.
-- |
-- | Registration returns synchronous cleanup for that receiver. Halo runs it
-- | when the subscription is removed or its React activation ends. A receiver
-- | remains bound to the activation that registered it, so a stale callback
-- | cannot dispatch into a later activation. Emitters do not provide a
-- | consuming queue or backpressure.
newtype Emitter action = Emitter
  ((action -> Effect Unit) -> Effect (Effect Unit))

-- | Create an emitter from registration logic. During deactivation, a throwing
-- | cleanup is isolated from the remaining scope cleanup and reported as
-- | `DeactivationError`.
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

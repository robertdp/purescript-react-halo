# Lifecycle and resources

One Halo runtime may pass through multiple React effect activations during development StrictMode. Handlers, forks, tasks, subscriptions, and registered cleanup belong to the activation that created them.

## Configure lifecycle handlers

Start with `defaultHandlers` and replace the callbacks the component uses:

```purescript
handlers = Halo.defaultHandlers
  { onActivate = initialize
  , onPropsChange = synchronizeProps
  , onAction = handleAction
  }
```

`onActivate` runs for every actual activation:

```purescript
onActivate =
  Task.once initializationSlot initialize
```

Development StrictMode may run setup, cleanup, then setup again for one hook instance. Treat activation as repeatable, not as an exactly-once mount event.

`onPropsChange` receives the previous props; read current props with `getProps`:

```purescript
onPropsChange previous = do
  current <- Halo.getProps
  synchronize previous current
```

`onAction` runs once for each dispatch in the active scope:

```purescript
onAction = case _ of
  Submitted -> submit
  Cancelled -> cancel
```

Each invocation is an independent root using the latest handlers and application interpreter supplied to the React boundary.

## Turn external events into actions

Create an emitter from registration logic. Registration returns synchronous cleanup for that receiver:

```purescript
names :: Halo.Emitter String
names = Halo.makeEmitter \emit ->
  source.listen emit
```

`Emitter` has a `Functor` instance, so map source values into the component action type before subscribing:

```purescript
actions :: Halo.Emitter Action
actions = NameChanged <$> names

onActivate =
  void $ Halo.subscribe actions
```

Mapping changes emitted values without changing source registration or cleanup. Emitters deliberately have no `Applicative` instance because combining event sources would require a synchronization policy such as zip or latest-value.

Use `subscribeWithId` when normal component flow must unsubscribe early:

```purescript
subscriptionId <- Halo.subscribeWithId \_ -> actions
Halo.unsubscribe subscriptionId
```

Manual unsubscribe removes tracking before running cleanup. Deactivation attempts cleanup for every subscription that remains registered. A retained callback stays bound to its original activation and cannot dispatch into a later StrictMode activation.

Emitters broadcast actions. They do not provide backpressure, consuming queues, or scheduling policies.

## Register synchronous cleanup

Use `registerCleanup` for a synchronous resource that is not an emitter subscription:

```purescript
cleanupId <- Halo.registerCleanup removeListener
```

Release it early when normal component flow no longer needs the resource:

```purescript
Halo.releaseCleanup cleanupId
```

Release removes the entry before invoking it. Unknown and already released IDs are ignored, and a throwing release is not retried during deactivation.

## Put asynchronous cleanup in an Aff finalizer

React effect cleanup is synchronous, so Halo intentionally has no asynchronous `onDeactivate` handler. Acquire and release an asynchronous resource inside owned Aff work:

```purescript
void $ Halo.fork do
  liftAff $ Aff.bracket acquire release use
```

Deactivation requests cancellation of the fork, which runs its Aff finalizer. React does not wait for that cancellation, so persistence or other required writes should happen during normal application flow rather than depending on unmount.

## Understand deactivation order

Deactivation first fences the activation. It then attempts synchronous registered and subscription cleanup before requesting cancellation of handlers, tasks, and forks.

```purescript
onError DeactivationError error =
  reportCleanupFailure error
```

One throwing cleanup does not prevent the remaining cleanup or cancellation requests. No ordering between generic and subscription cleanup is part of the API.

A stale root cannot commit component state, register another Halo capability, or begin a newly lifted application effect after the fence—even if it catches the initial Aff cancellation.

## Report unexpected errors

Both React entry points require:

```purescript
onError :: ErrorContext props action -> Error -> Effect Unit
```

Pattern match on the context when reporting needs different metadata:

```purescript
onError context error = case context of
  ActivationError -> report "activation" error
  PropsChangeError previous -> reportProps previous error
  ActionError action -> reportAction action error
  ForkError forkId -> reportFork forkId error
  DeactivationError -> report "cleanup" error
```

Halo reads the latest callback when an unexpected current failure is reported. Expected domain failures belong in application values, actions, or task outcomes. Halo-initiated cancellation is suppressed after its root is fenced.

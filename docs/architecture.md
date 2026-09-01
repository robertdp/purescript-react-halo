# Halo architecture

This document describes the ownership and safety invariants that maintainers must preserve. For public usage, start with the [README](../README.md) and [guide](guide.md).

## Application effects cross one boundary

Halo keeps application logic in an application monad `m`, commonly an `AppM` built on `ReaderT environment Aff`. `component` and `useHalo` receive a natural transformation in one direction:

```purescript
m ~> Aff
```

`HaloM props state action m` is a `ReaderT` over an internal execution record in `Aff`. Standard `lift` first checks that the execution still owns a current root, then invokes the root's captured interpreter. Derived `MonadEffect`, `MonadAff`, `MonadAsk`, `MonadTell`, and `MonadThrow` capabilities all pass through `m` and therefore use the same fence. Halo component state remains the separate `MonadState state` capability.

The interpreter must return the owned `Aff` computation. An interpreter that detaches work with `launchAff_` moves that work outside Halo's cancellation boundary.

## React delegates ownership to the runtime

[`React.Halo.Component`](../src/React/Halo/Component.purs) computes initial state from the initial props and delegates to `useHalo`. The renderer receives current props, Halo state, its coherent immutable task view, and an action dispatcher.

[`React.Halo.Hook`](../src/React/Halo/Hook.purs) creates one internal runtime for the hook instance and connects it to React effects:

1. synchronize the latest interpreter, handlers, error callback, and React state setter;
2. activate the runtime and return synchronous deactivation as effect cleanup; and
3. publish prop changes to the runtime.

The hook returns current `state`, an immutable task-authority `tasks` view, and `dispatch`. State and task view are published through one React setter as a coherent snapshot. [`React.Halo.Internal.Runtime`](../src/React/Halo/Internal/Runtime.purs) owns fibers, activation scopes, task authority, subscriptions, state fencing, and error routing.

## Each activation has a generation

An active runtime holds one scope with a unique generation, an active flag, and maps for handler roots, component forks, task authority, generic cleanup, and subscriptions. Activation is idempotent while that scope is current. Heterogeneous task-slot bindings persist separately for the runtime lifetime; active authority does not.

Deactivation marks the scope inactive and clears it from the runtime before foreign cleanup or Aff cancellation begins. A later activation creates fresh maps and a new generation. Currency checks require both an active matching generation and a live root owner, so work retained from an earlier generation cannot affect a reactivated component.

This generation boundary models React development StrictMode's setup-cleanup-setup sequence. `onActivate` runs once for every actual activation; cleanup fences the earlier generation before the replayed activation starts.

## Handlers and forks are roots

Every `onActivate`, `onPropsChange`, and `onAction` invocation starts an independently owned handler root. A new handler reads the current interpreter and latest handlers from the runtime. Preparation creates its owner and gated fiber; the runtime records the root before opening the gate, so immediate completion cannot race registration.

`fork` creates another activation-owned root with an opaque `ForkId`. It has its own liveness fence and may outlive the handler that launched it. The fork inherits the launching root's interpreter snapshot, even if hook synchronization supplied a newer interpreter before the call to `fork`. Unrelated handlers started after synchronization use the newer interpreter.

Root completion removes only that root's current map entry. IDs are fresh within the runtime, so stale completion cannot remove newer work.

Managed task policies use the same fork map and root ownership rather than a separate coordination subsystem. A `Task.Slot` combines a type-level brand with a canonical lens. First use registers an erased binding that can inspect and normalize that focus without retaining a body, input, or fork handle. Temporary semantic probes verify that one brand maps to one focus and one focus to one brand; probes are never published. A collision throws in the calling root's existing error context before state mutation or cancellation.

Activation authority maps a registered slot brand to an exact token containing runtime identity, activation generation, and `ForkId`. In one synchronous transaction the runtime reconciles registered slots, claims authority, fences a superseded root, prepares and records the replacement behind a gate, publishes state and view, requests old cancellation, and opens the new gate. The task body inherits the launching root's interpreter.

`Task.State` is freely copyable and does not itself prove ownership. `Task.View` is an immutable pair of the published state snapshot and authority map. Slot-aware projection reports `Active` only when the canonical focus and authority contain the exact token; copied, stale, cross-slot, and cross-runtime active values report `Idle`.

Every generic `MonadState` write reconciles registered bindings before publication. Preserving the exact token is ordinary. Replacing it with correctly branded idle or terminal state removes/fences authority while preserving the requested value. Foreign or stale active state is normalized to branded idle and can never supply a fork to cancel. Typed completion and unexpected failure use exact canonical-focus and authority checks.

Debounce timing remains private: an owned Aff timer precedes the body in the same managed root, and both phases project `Active`. A typed `Either` completion removes authority and stores `Failed` or `Succeeded` atomically. An unexpected current failure clears the exact run to `Idle` before existing `ForkError` routing.

## Fences precede cancellation

Cancellation is cooperative, but ownership loss is synchronous.

For explicit `kill`, the runtime removes the fork from tracking, fences its owner, requests Aff cancellation, and waits for the fiber and its Aff finalizers before returning. An unknown or completed `ForkId` is a no-op.

React deactivation cannot wait asynchronously. It invalidates the scope; takes all tracked roots, generic cleanup, and subscriptions; and fences every root. Activation authority is cleared and persistent bindings normalize active task state to `Idle` in the runtime state without calling React's setter during cleanup. Every synchronous cleanup is attempted before cancellation is requested for handler, fork, and task fibers. Cleanup failures are reported only after the runtime has attempted the rest of the cleanup work.

If deactivation normalized task state, the next activation publishes that runtime state through the latest React setter before starting `onActivate`. Terminal task outcomes have no active root and persist. This ordering keeps StrictMode replay state coherent without adding an asynchronous deactivation callback.

The fences protect two important boundaries:

- `MonadState` may still compute a stale operation's return value, but it cannot update stored state or call React's state setter.
- A later `lift` from a stale root fails with Halo's internal cancellation error before invoking `m ~> Aff`. Catching the initial Aff cancellation therefore cannot start a newly lifted application effect.

Capabilities that create or remove forks, task roots, cleanup, and subscriptions also check currency. Cancellation cannot undo an external effect that already happened inside an application computation; application writes must still use appropriate idempotency or retry semantics.

## Synchronous cleanup stays activation-scoped

Generic cleanup and emitter subscription cleanup use separate activation maps. `registerCleanup` stores an `Effect Unit`; `releaseCleanup` removes it before invocation. Deactivation takes both maps and attempts each disposer independently, so one throw cannot block another disposer or root cancellation. Deactivation failures share `DeactivationError`, but cross-category cleanup ordering is not a public contract.

Cleanup IDs are runtime-fresh and a release consults only the current scope. An ID or stale root retained from one StrictMode activation therefore cannot remove resources from another.

## Subscriptions close over their activation

The local [`Emitter`](../src/React/Halo/Subscription.purs) registers an `Effect` callback and returns a synchronous cleanup. The scope tracks that cleanup by `SubscriptionId`. Manual unsubscription removes the entry before running cleanup, which prevents a throwing cleanup from being retried during deactivation.

Deactivation takes the complete subscription map and attempts each cleanup independently. A retained emitter callback still dispatches through its original scope; the generation check rejects it after deactivation, including after StrictMode reactivation.

## Parallel branches share ownership

`HaloAp` is the direct parallel counterpart to `HaloM`: it changes the internal `ReaderT` result from `Aff` to `ParAff` without creating another ownership model. Parallel branches share one root owner, activation scope, error context, and interpreter snapshot. The surrounding computation waits for the branches when it returns to `HaloM`.

Concurrent Halo state writes have nondeterministic ordering and can overwrite one another. Prefer running independent application reads in parallel, combining their results, and committing Halo state once.

## Errors use the current reporting callback

Each root carries the context assigned at launch: `ActivationError`, `PropsChangeError previousProps`, `ActionError action`, or `ForkError id`. Managed task bodies use `ForkError` because they are component-owned roots without separate public run identity. An unexpected failure is reported only while that root and scope are still current. The runtime reads the latest `onError` callback at reporting time, so a render can update reporting without changing an already-running root's interpreter.

Subscription cleanup failures use `DeactivationError`. Halo-initiated cancellation and stale-lift failures are suppressed because the owner has already been fenced.

## Tests protect the invariant boundaries

The deterministic runtime tests exercise ownership without mounting a real DOM fixture:

- [`RuntimeSpec`](../test/Test/Halo/RuntimeSpec.purs) covers AppM interpretation, handler and fork interpreter snapshots, the stale-lift fence, and direct parallel execution.
- [`ScopeHandlerSpec`](../test/Test/Halo/ScopeHandlerSpec.purs) covers activation generations, StrictMode reactivation, props, handler and fork ownership, explicit kill, finalizer waiting, and stale capability/state rejection.
- [`SubscriptionErrorSpec`](../test/Test/Halo/SubscriptionErrorSpec.purs) covers generic and subscription cleanup isolation, manual release, stale activation IDs and emitter callbacks, error contexts, and latest error-handler selection.
- [`TaskSpec`](../test/Test/Halo/TaskSpec.purs) covers branded slots, view projection, collision detection, state-copy and stale-snapshot reconciliation, cross-runtime authority, policies, atomic claims, supersession and reset, private debounce scheduling, stale effect fences, two-slot deactivation normalization, current setters, and inherited interpreters.
- [`DocExamples`](../test/Test/Halo/DocExamples.purs) compile-checks the complete component, hook, AppM, and task example.
- [`GuideExamples`](../test/Test/Halo/GuideExamples.purs) compile-checks the guide's task, parallel, subscription, and cleanup examples.

[`test/Main.purs`](../test/Main.purs) runs the full behavioral suite. Preserve these deterministic boundaries when changing the runtime; add a focused regression beside the invariant it protects.

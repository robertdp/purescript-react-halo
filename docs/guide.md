# Halo guide

Halo connects a React entry point to typed actions, component state, and application effects. Begin with the entry point your component needs, then add stronger ownership only when an interaction requires it.

## Start here

- Use [`Halo.component`](guide/getting-started.md#use-component-for-a-complete-component) when Halo owns the complete component.
- Use [`Halo.useHalo`](guide/getting-started.md#use-usehalo-with-other-hooks) when the render function also uses other hooks.

The [getting-started chapter](guide/getting-started.md) builds both forms from the React boundary inward: initial state, rendering, dispatch, handlers, AppM, and error reporting.

## Choose the next chapter

### [Actions, effects, and state](guide/actions-and-state.md)

Use this chapter to answer:

- How does `dispatch` reach `onAction`?
- How do I run AppM logic with `lift`?
- How do overlapping handlers affect state?
- How do I read current props or run independent effects in parallel?

### [Managed work](guide/managed-work.md)

Use this chapter to choose among handler-owned work, tasks, and forks:

| Requirement | Mechanism |
|---|---|
| Work belongs to one dispatched action | handler |
| Rendering needs typed lifecycle state | task |
| Work must outlive its launching handler | fork |
| Cancellation must await finalizers | `Task.reset` or `Halo.kill` |

### [Lifecycle and resources](guide/lifecycle-and-resources.md)

Use this chapter for activation, StrictMode replay, subscriptions, cleanup, and unexpected errors.

```text
component or useHalo
  └─ action handler
       ├─ ordinary HaloM work
       ├─ managed task
       └─ component-owned fork
```

Every handler, task, fork, subscription, and registered cleanup belongs to one React activation. Deactivation fences that activation before cleanup and cancellation requests begin.

Maintainers changing these guarantees should also read the [runtime architecture](architecture.md).

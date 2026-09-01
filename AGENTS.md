# Repository instructions for agents

Use this file as the repository control plane. Do not treat it as a substitute for the reader and maintainer documentation.

## Read the relevant sources first

For an unfamiliar task, route yourself by scope:

- Read [README.md](README.md) for the product boundary and current public mental model.
- Read [docs/guide.md](docs/guide.md) for supported usage and cancellation guidance.
- Read [docs/architecture.md](docs/architecture.md) before changing runtime ownership, interpreters, concurrency, subscriptions, or error handling.
- Read [CONTRIBUTING.md](CONTRIBUTING.md) for setup, validation, and pull request readiness.
- Inspect [`React.Halo`](src/React/Halo.purs) and the public module that owns an API before changing its contract.
- Inspect the corresponding modules under `test/Test/Halo/` before changing behavior; tests are executable contracts for runtime invariants.

Before changing dependencies or developer commands, inspect `package.json`, `spago.yaml`, and `.github/workflows/ci.yml`. `package.json` owns tool pins and scripts; `spago.yaml` owns PureScript dependencies and the package set; CI owns automated pull request checks.

## Preserve repository boundaries

- Keep `React.Halo` as the intentional public API root. Do not expose internal runtime ownership types as a shortcut.
- Keep React integration in the component/hook boundary and runtime ownership in `React.Halo.Internal.Runtime`. Read the architecture document instead of duplicating its rules here.
- Keep application capabilities routed through the application monad and its `m ~> Aff` interpreter.
- Do not add npm runtime dependencies or an npm runtime entry point. The npm package is development tooling only.
- Update public docs, compile-checked examples, and focused deterministic tests when public behavior changes.
- Do not hand-edit or commit `generated-docs/`, `output/`, `.spago/`, or `node_modules/`; they are ignored generated or dependency state.

## Validate completion

Use focused checks while iterating. Before declaring a repository change complete, run the full sequence in [CONTRIBUTING.md](CONTRIBUTING.md): format check, strict and pedantic build, full tests, and docs generation. Review the final diff, verify local documentation links, and run a whitespace check. Report any skipped, failed, or unavailable validation precisely.

There is no real DOM fixture. Describe successful runtime tests and compile checks accurately; do not claim browser mounting coverage.

## Require explicit approval for external actions

Do not push commits, publish packages or documentation, create a release, edit GitHub or pull request state, or change any other external resource without current, action-specific authorization. Repository change approval does not imply release or publication approval.

# Contributing

This guide covers local setup, validation, and pull request readiness. Start with the [README](README.md) for the public mental model, use the [guide index](docs/guide.md) to find the relevant usage chapter, and read the [architecture notes](docs/architecture.md) before changing runtime ownership or cancellation behavior.

## Set up the checkout

Use Node.js 22 with npm to match CI, then install the locked development tools:

```console
npm ci
```

`package.json` is the source of truth for tool pins and npm scripts. `spago.yaml` owns the PureScript dependencies and package set. CI owns the automated checks run for pull requests.

Halo has no npm runtime entry point or npm runtime dependencies. Do not add an npm runtime dependency to provide behavior that belongs in PureScript or an existing PureScript package. Add or change PureScript dependencies in `spago.yaml`.

## Develop and validate

During implementation, run the smallest relevant build or test that gives useful feedback. Changes to runtime ownership, cancellation, subscriptions, interpreters, or public behavior need focused regression coverage in the corresponding test module.

Before marking a pull request ready, run the full local validation sequence from the repository root:

```console
npm run format:check
npm run build -- --strict --pedantic-packages
npm test
npx spago docs
```

Use `npm run format` to apply the repository formatter when the format check fails. A focused check helps iteration but does not replace the full sequence before review.

The documentation command writes generated API pages to `generated-docs/`. Spago also writes build and dependency state to `output/` and `.spago/`. These paths are ignored; do not hand-edit or commit them.

## Keep behavior, tests, and docs aligned

`React.Halo` is the public API root; the generic state-focused names in `React.Halo.Task` form a separate qualified surface. When a change affects either module's exported types or behavior:

- update the relevant public module documentation;
- update the README or guide when their guidance changes, and keep exact API contracts in public source comments used by generated documentation;
- keep examples in `test/Test/Halo/DocExamples.purs` compiling; and
- add or update deterministic tests for the changed invariant.

Use [docs/architecture.md](docs/architecture.md) to find the runtime contract and the tests that protect it. The runtime tests exercise ownership directly without a real DOM fixture, so report validation as runtime or compile coverage rather than as a mounted-browser test.

Documentation-only changes still require link inspection and documentation generation. Run the complete validation sequence when preparing the pull request so CI-facing code, examples, and package checks remain covered.

## Check pull request readiness

Before requesting review, confirm that:

- the change is focused and its public effect is clear;
- formatting, strict and pedantic build, full tests, and docs generation pass;
- public API changes include matching documentation and tests;
- generated or dependency output is not staged; and
- the final diff contains only intended files and no whitespace errors.

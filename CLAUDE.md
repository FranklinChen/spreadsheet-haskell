# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Idiomatic Haskell translation of a reactive spreadsheet/dataflow system from [Neel Krishnaswami's blog post](https://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html). The OCaml reference implementation is [FranklinChen/spreadsheet-ocaml](https://github.com/FranklinChen/spreadsheet-ocaml). Changes should be faithful to that original in spirit.

## Build & Test Commands

```bash
cabal build all          # build
cabal test all           # run tests
haskell-ci regenerate    # regenerate CI workflow after changing tested-with
```

GHC2024 language edition; requires GHC 9.10+. Flag `-Wall` is enabled project-wide. Formatting: Fourmolu.

## Architecture

Single-module library (`src/Spreadsheet/Cell.hs`) using IO + IORef for mutable state with functional composability via a custom `Exp` monad:

- **`Cell a`** — mutable cell holding an expression, cached value, dependency graph edges (`reads`/`observers`), and a unique ID
- **`Exp a`** — monadic expression type that tracks which cells are read during evaluation (returns `Result a [ECell]`)
- **`ECell`** — existential wrapper (GADTs) allowing heterogeneous cell collections
- **`cell`** — construct a new cell from an expression
- **`get`** — read a cell's value with automatic dependency tracking
- **`set`** — update a cell's expression and recursively invalidate observers
- **`run`** — evaluate an `Exp` to get its result value

Key pattern: lazy memoization via IORef with graph-based invalidation propagation. Uses `OverloadedRecordDot` + `NoFieldSelectors` for OCaml-like field access (`c.value`).

## Tests

HSpec tests in `test/Spreadsheet/CellSpec.hs` (core scenarios) and `test/Spreadsheet/CellExtraSpec.hs` (edge cases: cycles, diamonds, chains, dynamic dependencies). Auto-discovery via `test/Spec.hs`.

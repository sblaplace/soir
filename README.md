# Soir 🌆

An ISO Prolog based WebAssembly runtime and library.

The library which the runtime executes based on is made to match as closely as
possible to the
[WebAssembly Core Specification](https://webassembly.github.io/spec/core/) with
adjustments being made minimally to accommodate for clean, valid prolog.

This runtime is still in its early stages, and feedback is very welcome. It is
my hope to be able to take advantage of prolog's natural affinity for tree
parsing to produce correct, fast execution of wasm modules. The design is still
open to being changed at any point, so please avoid using this in any critical
applications. If you have thoughts on the design, open an issue so we can
discuss them.

In terms of Prolog implementations, this is tested against Scryer but should
function with any ISO compliant implementation that ships with the relevant
libraries.

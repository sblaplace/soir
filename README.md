# Soir 🌆

A Prolog based WebAssembly runtime and library. It is ISO compliant, and tested
against Scryer Prolog.

The library which the runtime executes based on is made to match as closely as
possible to the
[WebAssembly Core Specification version 1.0](https://www.w3.org/TR/2019/REC-wasm-core-1-20191205)
with adjustments being made minimally to accommodate for clean prolog. In the
future, the
[WebAssembly Core Specification version 2.0 Working Draft](https://www.w3.org/TR/wasm-core-2/)
will also be added as an option, before eventually being made the default once
it becomes a recommendation.

This runtime is still in its early stages, and feedback is very welcome. It is my hope to be able
to take advantage of prolog's natural affinity for tree parsing to produce correct, fast execution
of wasm modules.

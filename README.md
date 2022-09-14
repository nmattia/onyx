# Onyx

Onyx is a typechecker for the [Nix](https://nixos.org/explore.html) language.

> **Warning**
>
> Onyx is very much a work in progress. only a subset of the Nix syntax is supported.

You can try Onyx online: [nmattia.github.io/onyx](https://nmattia.github.io/onyx/)

## Build

This repo contains both the Onyx library for the type system and typechecking, and the online [playground](./playground).

To build and test the library, run the following:

```bash
cargo test
```

To build the Wasm module for the playground, you will need [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/); then run the following command:

```bash
cd playground/wasm && wasm-pack build
```

To run the actual playground app, make sure you've been the Wasm module for the playground and then run the following command:

```bash
cd playground/app && npm ci && npm run dev
```

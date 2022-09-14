<script lang="ts">
  import { check } from "onyx-wasm";

  const wrapError = (s: string) => {
    if (s === "") {
      return "";
    }
    try {
      return check(s);
    } catch (e: any) {
      return `Error: ${e.toString()}`;
    }
  };

  let input = "";
  $: output = wrapError(input);
</script>

<main id="container">
  <h1>Onyx</h1>
  <p>
    Onyx is a typechecker for the <a href="https://nixos.org/explore.html"
      >Nix</a
    >
    language. Onyx is very much a work in progress. See the
    <a href="https://github.com/nmattia/onyx">GitHub repo</a>
    for more info.<br />
  </p>
  <p>
    Try writing some Nix code below, and Onyx will try to infer the type. Only a
    small subset of the Nix syntax is currently supported.
  </p>

  <textarea
    on:input={(e) => {
      input = e.target.value;
    }}
    style="width: 100%;"
  />
  <output>{output}</output>
</main>

<style>
  #container {
    margin: auto;
    max-width: 600px;
    background: aliceblue;
  }
</style>

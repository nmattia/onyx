<script lang="ts">
  import { check } from "onyx-wasm";
  import { EditorView } from "codemirror";
  import { lineNumbers } from "@codemirror/view";

  let codemirrorContainer = null;
  let output = "";

  function wrapError(s: string): string {
    if (s === "") {
      return "";
    }
    try {
      return check(s);
    } catch (e: any) {
      return `Error: ${e.toString()}`;
    }
  }

  function setupEditor(s: Element): void {
    new EditorView({
      doc: `let
  x = 2;
  f = x /* integer */: add x 42;
in f x`,
      extensions: [
        EditorView.updateListener.of((update) => {
          let text = update.state.doc.toString();
          output = wrapError(text);
        }),
        lineNumbers(),
      ],
      parent: s,
    });
  }

  $: setupEditor(codemirrorContainer);
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
    Type some Nix code below, and Onyx will try to infer the type. Only a small
    subset of the Nix syntax is currently supported.
  </p>

  <div class="cm-container" bind:this={codemirrorContainer} />
  <output>{output}</output>

  <div>
    <a
      href="https://github.com/nmattia/onyx/issues/new/choose"
      target="_blank"
      rel="noopener noreferrer">Open ticket</a
    >
  </div>
</main>

<style>
  #container {
    margin: auto;
    max-width: 600px;
    background: aliceblue;
    padding: 1rem;
  }

  :global(.cm-content) {
    background: white;
    margin: 1rem;
    border: 1px solid black;
  }
</style>

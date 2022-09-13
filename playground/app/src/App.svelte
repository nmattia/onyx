<script lang="ts">
    import { check } from "onyx-wasm";

    const wrapError = (s: string) => {
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
  <textarea on:input={ (e) => { input = e.target.value; } } style="width: 100%;"> </textarea>
  <output>{output}</output>
</main>


<style>
    #container {
        margin: auto;
        max-width: 600px;
        background: aliceblue;
    }
</style>

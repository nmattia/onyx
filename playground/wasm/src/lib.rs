use wasm_bindgen::prelude::*;

use onyx::ast::parse;
use onyx::typecheck::synthesize;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn check(input: &str) -> Result<String, String> {
    console_error_panic_hook::set_once();
    let res = parse(input)?;
    let res = synthesize(&res);

    Ok(format!("{}", res))
}

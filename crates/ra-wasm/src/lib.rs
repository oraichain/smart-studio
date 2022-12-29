#![cfg(target_arch = "wasm32")]

use wasm_bindgen::prelude::*;

use rust_pack::{extractor, state::LocalState};

pub use wasm_bindgen_rayon::init_thread_pool;

#[wasm_bindgen(start)]
pub fn start() {
    console_error_panic_hook::set_once();
    log::info!("worker initialized")
}

#[wasm_bindgen]
pub struct WorldState {
    state: LocalState,
}

#[wasm_bindgen]
impl WorldState {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { state: LocalState::default() }
    }

    pub fn load(&mut self, json: Vec<u8>) {
        let json = String::from_utf8_lossy(&json);
        let change = extractor::load_change_from_json(&json);

        // apply change
        self.state.apply_change(change)
    }

    pub fn init(
        &mut self,
        rust_std: String,
        rust_core: String,
        rust_alloc: String,
        rust_cosmwasm_derive: String,
        rust_cosmwasm_schema_derive: String,
        rust_cosmwasm_schema: String,
        rust_cosmwasm_std: String,
        rust_cosmwasm_crypto: String,
        rust_cosmwasm_storage: String,
        rust_thiserror: String,
        rust_thiserror_impl: String,
        // rust_proc_macro2: String,
    ) {
        let change = extractor::load_change_from_files(
            rust_std,
            rust_core,
            rust_alloc,
            rust_cosmwasm_derive,
            rust_cosmwasm_schema_derive,
            rust_cosmwasm_schema,
            rust_cosmwasm_std,
            rust_cosmwasm_crypto,
            rust_cosmwasm_storage,
            rust_thiserror,
            rust_thiserror_impl,
            // rust_proc_macro2,
        );

        // apply change
        self.state.apply_change(change)
    }

    pub fn update(&mut self, file_ind: u32, code: String, with_highlight: Option<bool>) -> JsValue {
        log::warn!("update");
        let ret = self.state.update(file_ind, code, with_highlight.unwrap_or_default());

        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn inlay_hints(&self, file_ind: u32) -> JsValue {
        let ret = self.state.inlay_hints(file_ind);
        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn completions(&self, file_ind: u32, line_number: u32, column: u32) -> JsValue {
        log::warn!("completions");
        let ret = self.state.completions(file_ind, line_number, column);
        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn hover(&self, file_ind: u32, line_number: u32, column: u32) -> JsValue {
        log::warn!("hover");
        if let Some(ret) = self.state.hover(file_ind, line_number, column) {
            return serde_wasm_bindgen::to_value(&ret).unwrap();
        }

        JsValue::NULL
    }

    pub fn code_lenses(&self, file_ind: u32) -> JsValue {
        log::warn!("code_lenses");
        let ret = self.state.code_lenses(file_ind);

        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn references(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
        include_declaration: bool,
    ) -> JsValue {
        log::warn!("references");
        let ret = self.state.references(file_ind, line_number, column, include_declaration);

        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn prepare_rename(&self, file_ind: u32, line_number: u32, column: u32) -> JsValue {
        log::warn!("prepare_rename");
        if let Some(ret) = self.state.prepare_rename(file_ind, line_number, column) {
            return serde_wasm_bindgen::to_value(&ret).unwrap();
        }
        JsValue::NULL
    }

    pub fn rename(&self, file_ind: u32, line_number: u32, column: u32, new_name: &str) -> JsValue {
        log::warn!("rename");
        let ret = self.state.rename(file_ind, line_number, column, new_name);

        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn signature_help(&self, file_ind: u32, line_number: u32, column: u32) -> JsValue {
        log::warn!("signature_help");
        if let Some(ret) = self.state.signature_help(file_ind, line_number, column) {
            return serde_wasm_bindgen::to_value(&ret).unwrap();
        }

        JsValue::NULL
    }

    pub fn definition(&self, file_ind: u32, line_number: u32, column: u32) -> JsValue {
        log::warn!("definition");
        let ret = self.state.definition(file_ind, line_number, column);
        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn type_definition(&self, file_ind: u32, line_number: u32, column: u32) -> JsValue {
        log::warn!("type_definition");
        let ret = self.state.type_definition(file_ind, line_number, column);
        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn document_symbols(&self, file_ind: u32) -> JsValue {
        log::warn!("document_symbols");
        let ret = self.state.document_symbols(file_ind);

        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn type_formatting(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
        ch: char,
    ) -> JsValue {
        log::warn!("type_formatting");
        let ret = self.state.type_formatting(file_ind, line_number, column, ch);

        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn folding_ranges(&self, file_ind: u32) -> JsValue {
        log::warn!("folding_ranges");
        let ret = self.state.folding_ranges(file_ind);

        serde_wasm_bindgen::to_value(&ret).unwrap()
    }

    pub fn goto_implementation(&self, file_ind: u32, line_number: u32, column: u32) -> JsValue {
        log::warn!("goto_implementation");
        let ret = self.state.goto_implementation(file_ind, line_number, column);

        serde_wasm_bindgen::to_value(&ret).unwrap()
    }
}

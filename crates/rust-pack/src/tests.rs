use std::fs;

use crate::extractor::{self, CONTRACT_FILES};
use crate::state::LocalState;

#[test]
fn initialize_state() {
    let mut state = LocalState::default();
    let json = fs::read_to_string("../../change.json").unwrap();
    println!("json length {}", json.len());
    let change = extractor::load_change_from_json(&json);
    state.apply_change(change);

    for id in 0..CONTRACT_FILES.len() {
        let code =
            fs::read_to_string(format!("../../templates/empty_contract{}", CONTRACT_FILES[id]))
                .unwrap();
        state.update(id as u32, code, false);
    }

    let ret = state.completions(4, 34, 4);
    assert!(ret.len() > 0);
}

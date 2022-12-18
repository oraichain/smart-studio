use std::fs;

use crate::extractor::CONTRACT_FILES;
use crate::state::LocalState;

#[test]
fn initialize_state() {
    let mut state = LocalState::default();
    let change_json = fs::read_to_string("../../change.json").unwrap();
    println!("json length {}", change_json.len());
    state.load(change_json.into_bytes());

    for id in 0..CONTRACT_FILES.len() {
        let code =
            fs::read_to_string(format!("../../templates/empty_contract{}", CONTRACT_FILES[id]))
                .unwrap();
        state.update(id as u32, code);
    }

    let ret = state.completions(4, 34, 4);
    assert!(ret.len() > 0);
}

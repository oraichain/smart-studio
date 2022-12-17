use std::fs;

use crate::state::LocalState;

#[test]
fn initialize_state() {
    let mut state = LocalState::new();
    let change_json = fs::read_to_string("../../change.json").unwrap();
    println!("json length {}", change_json.len());
    state.load(change_json.into_bytes());

    let code = fs::read_to_string("../../templates/empty_contract/src/contract.rs").unwrap();
    println!("code length {}", code.len());
    let ret = state.update(0, code);

    println!("{:?}", ret.highlights.first().unwrap().tag);

    let ret = state.completions(0, 34, 4);
    assert!(ret.len() > 0);
}

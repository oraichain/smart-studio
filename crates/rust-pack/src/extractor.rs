use std::sync::Arc;

use cfg::CfgOptions;
use ide::{Change, CrateGraph, CrateId, Edition, FileId, SourceRoot};

use ide_db::base_db::{CrateName, Dependency, Env, FileSet, VfsPath};

use change_json::ChangeJson;

pub const CONTRACT_FILE_PATH: &str = "/contract_crate/main.rs";

pub fn create_crate(crate_graph: &mut CrateGraph, f: FileId) -> CrateId {
    let mut cfg = CfgOptions::default();
    cfg.insert_atom("unix".into());
    cfg.insert_key_value("target_arch".into(), "x86_64".into());
    cfg.insert_key_value("target_pointer_width".into(), "64".into());
    crate_graph.add_crate_root(
        f,
        Edition::Edition2018,
        None,
        None,
        cfg,
        Default::default(),
        Env::default(),
        Vec::new(),
    )
}

pub fn create_source_root(name: &str, f: FileId) -> SourceRoot {
    let mut file_set = FileSet::default();
    file_set.insert(f, VfsPath::new_virtual_path(format!("/{}/src/lib.rs", name)));
    SourceRoot::new_library(file_set)
}

pub fn load_change_from_json(json: &str) -> Change {
    let change: ChangeJson =
        serde_json::from_str(json).expect("`Change` deserialization must work");
    Change::from(change)
}

pub fn load_change_from_files(
    rust_std: String,
    rust_core: String,
    rust_alloc: String,
    rust_cosmwasm_derive: String,
    rust_cosmwasm_schema_derive: String,
    rust_cosmwasm_schema: String,
    rust_cosmwasm_std: String,
    rust_cosmwasm_crypto: String,
    rust_cosmwasm_storage: String,
) -> Change {
    let file_id = FileId(0);
    let std_id = FileId(1);
    let core_id = FileId(2);
    let alloc_id = FileId(3);
    let cosmwasm_derive_id = FileId(4);
    let cosmwasm_schema_derive_id = FileId(5);
    let cosmwasm_schema_id = FileId(6);
    let cosmwasm_std_id = FileId(7);
    let cosmwasm_crypto_id = FileId(8);
    let cosmwasm_storage_id = FileId(9);

    let mut file_set = FileSet::default();
    file_set.insert(file_id, VfsPath::new_virtual_path(CONTRACT_FILE_PATH.to_string()));
    let source_root = SourceRoot::new_local(file_set);

    let mut change = Change::new();
    change.set_roots(vec![
        source_root,
        create_source_root("std", std_id),
        create_source_root("core", core_id),
        create_source_root("alloc", alloc_id),
        create_source_root("cosmwasm_derive", cosmwasm_derive_id),
        create_source_root("cosmwasm_schema_derive", cosmwasm_schema_derive_id),
        create_source_root("cosmwasm_schema", cosmwasm_schema_id),
        create_source_root("cosmwasm_std", cosmwasm_std_id),
        create_source_root("cosmwasm_crypto", cosmwasm_crypto_id),
        create_source_root("cosmwasm_storage", cosmwasm_storage_id),
    ]);
    let mut crate_graph = CrateGraph::default();
    let contract_crate = create_crate(&mut crate_graph, file_id);
    let std_crate = create_crate(&mut crate_graph, std_id);
    let core_crate = create_crate(&mut crate_graph, core_id);
    let alloc_crate = create_crate(&mut crate_graph, alloc_id);
    let cosmwasm_derive_crate = create_crate(&mut crate_graph, cosmwasm_derive_id);
    let cosmwasm_schema_derive_crate = create_crate(&mut crate_graph, cosmwasm_schema_derive_id);
    let cosmwasm_schema_crate = create_crate(&mut crate_graph, cosmwasm_schema_id);
    let cosmwasm_std_crate = create_crate(&mut crate_graph, cosmwasm_std_id);
    let cosmwasm_crypto_crate = create_crate(&mut crate_graph, cosmwasm_crypto_id);
    let cosmwasm_storage_crate = create_crate(&mut crate_graph, cosmwasm_storage_id);

    let core_dep = Dependency::new(CrateName::new("core").unwrap(), core_crate);
    let alloc_dep = Dependency::new(CrateName::new("alloc").unwrap(), alloc_crate);
    let std_dep = Dependency::new(CrateName::new("std").unwrap(), std_crate);
    let cosmwasm_derive_dep =
        Dependency::new(CrateName::new("cosmwasm_derive").unwrap(), cosmwasm_derive_crate);
    let cosmwasm_schema_derive_dep = Dependency::new(
        CrateName::new("cosmwasm_schema_derive").unwrap(),
        cosmwasm_schema_derive_crate,
    );
    let cosmwasm_schema_dep =
        Dependency::new(CrateName::new("cosmwasm_schema").unwrap(), cosmwasm_schema_crate);
    let cosmwasm_std_dep =
        Dependency::new(CrateName::new("cosmwasm_std").unwrap(), cosmwasm_std_crate);
    let cosmwasm_crypto_dep =
        Dependency::new(CrateName::new("cosmwasm_crypto").unwrap(), cosmwasm_crypto_crate);
    let cosmwasm_storage_dep =
        Dependency::new(CrateName::new("cosmwasm_storage").unwrap(), cosmwasm_storage_crate);

    // dependencies
    crate_graph.add_dep(std_crate, core_dep.clone()).unwrap();
    crate_graph.add_dep(std_crate, alloc_dep.clone()).unwrap();
    crate_graph.add_dep(alloc_crate, core_dep.clone()).unwrap();
    crate_graph.add_dep(cosmwasm_std_crate, core_dep.clone()).unwrap();
    crate_graph.add_dep(cosmwasm_std_crate, cosmwasm_derive_dep.clone()).unwrap();
    crate_graph.add_dep(cosmwasm_storage_crate, cosmwasm_std_dep.clone()).unwrap();
    crate_graph.add_dep(cosmwasm_schema_crate, cosmwasm_schema_derive_dep.clone()).unwrap();

    crate_graph.add_dep(contract_crate, core_dep).unwrap();
    crate_graph.add_dep(contract_crate, alloc_dep).unwrap();
    crate_graph.add_dep(contract_crate, std_dep).unwrap();
    crate_graph.add_dep(contract_crate, cosmwasm_derive_dep).unwrap();
    crate_graph.add_dep(contract_crate, cosmwasm_schema_derive_dep).unwrap();
    crate_graph.add_dep(contract_crate, cosmwasm_schema_dep).unwrap();
    crate_graph.add_dep(contract_crate, cosmwasm_std_dep).unwrap();
    crate_graph.add_dep(contract_crate, cosmwasm_crypto_dep).unwrap();
    crate_graph.add_dep(contract_crate, cosmwasm_storage_dep).unwrap();

    change.change_file(std_id, Some(Arc::new(rust_std)));
    change.change_file(core_id, Some(Arc::new(rust_core)));
    change.change_file(alloc_id, Some(Arc::new(rust_alloc)));
    change.change_file(cosmwasm_derive_id, Some(Arc::new(rust_cosmwasm_derive)));
    change.change_file(cosmwasm_schema_derive_id, Some(Arc::new(rust_cosmwasm_schema_derive)));
    change.change_file(cosmwasm_schema_id, Some(Arc::new(rust_cosmwasm_schema)));
    change.change_file(cosmwasm_std_id, Some(Arc::new(rust_cosmwasm_std)));
    change.change_file(cosmwasm_crypto_id, Some(Arc::new(rust_cosmwasm_crypto)));
    change.change_file(cosmwasm_storage_id, Some(Arc::new(rust_cosmwasm_storage)));

    change.set_crate_graph(crate_graph);

    change
}

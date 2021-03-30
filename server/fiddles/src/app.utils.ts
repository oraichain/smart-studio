import fs from 'fs';
import fse from 'fs-extra';
import { Request } from 'express';
import path from 'path';
// import TOML, { JsonMap } from '@iarna/toml';
import shell from 'shelljs';

export const smartContractFolder = process.env.CONTRACT_FOLDER || '/code';
export const smartContractPackages = path.join(smartContractFolder, 'packages');

export const getFiles = (dir: string, results = []): string[] => {
  const dirents = fs.readdirSync(dir, { withFileTypes: true });

  for (const dirent of dirents) {
    const res = path.resolve(dir, dirent.name);
    if (dirent.isDirectory()) {
      if (dirent.name !== 'target') {
        getFiles(res, results);
      }
    } else {
      // do not push hidden files
      if (!isHiddenFiles(res)) results.push(res);
    }
  }
  return results;
};

// export const getFileSize = (size: number): string => {
//   const fileSize = size.toString();
//   if (fileSize.length < 4) return `${fileSize} bytes`;
//   if (fileSize.length < 7)
//     return `${Math.round(+fileSize / 1024).toFixed(2)} kb`;
//   return `${(Math.round(+fileSize / 1024) / 1000).toFixed(2)} MB`;
// };

export const filterPath = (name: any): string => {
  const [base, ext] = name.toString().split('.');

  let filePath = base
    .replace(/[^\w\/]/g, '')
    .replace(/\/{2,}/g, '/')
    .replace(/^\//, '');
  if (ext) {
    filePath = `${filePath}.${ext}`;
  }
  return filePath;
};

export const filterName = (name: any): string => {
  return name.toString().replace(/[^\w]/g, '');
};

// export interface WorkSpace extends JsonMap {
//   workspace?: {
//     members: string[];
//   };
// }

export const isHiddenFiles = (filePath: string): boolean => {
  if (filePath.endsWith('Cargo.toml') || filePath.endsWith('schema.rs')) {
    return true;
  }
  return false;
};

export interface IBuildFiddleResponse {
  message: string;
  success: boolean;
}

export const getUserPrefix = (user?: Express.User) => {
  return user ? user.username : 'guest';
};

export class SmartContractUtils {
  private userPrefix: string;

  constructor(request: Request) {
    this.userPrefix = getUserPrefix(request.user);
  }

  public initProject(name: string) {
    const contractName = `${this.userPrefix}_${name}`;
    const contractSub = path.join(this.userPrefix, name);

    const toml = `[package]
edition = "2018"
name = "${contractName}"
version = "0.1.0"

exclude = [
  # Those files are rust-optimizer artifacts. You might want to commit them for convenience but they should not be part of the source code publication.
  "contract.wasm",
  "hash.txt",
]

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html
[lib]
crate-type = ["cdylib", "rlib"]

[profile.release]
codegen-units = 1
debug = false
debug-assertions = false
incremental = false
lto = true
opt-level = 3
overflow-checks = true
panic = 'abort'
rpath = false

[features]
# for more explicit tests, cargo test --features=backtraces
backtraces = ["cosmwasm-std/backtraces"]

[dependencies]
cosmwasm-std = {version = "0.11.0"}
cosmwasm-storage = { version = "0.11.0" }
schemars = "0.7"
serde = {version = "1.0.103", default-features = false, features = ["derive"]}
thiserror = {version = "1.0.21"}

[dev-dependencies]
cosmwasm-schema = {version = "0.11.0"}`;

    fs.writeFileSync(
      path.join(smartContractPackages, contractSub, 'Cargo.toml'),
      toml,
    );

    const schema = `
use std::env::current_dir;
use std::fs::create_dir_all;

use cosmwasm_schema::{export_schema, remove_schemas, schema_for};

use ${contractName}::msg::{HandleMsg, InitMsg, QueryMsg};

fn main() {
    let mut out_dir = current_dir().unwrap();
    out_dir.push("packages/${contractSub}/artifacts/schema");
    create_dir_all(&out_dir).unwrap();
    remove_schemas(&out_dir).unwrap();

    export_schema(&schema_for!(InitMsg), &out_dir);
    export_schema(&schema_for!(HandleMsg), &out_dir);
    export_schema(&schema_for!(QueryMsg), &out_dir);
}
`;

    // write and create folder if not existed
    fse.outputFileSync(
      path.join(smartContractPackages, contractSub, 'examples', 'schema.rs'),
      schema,
    );
  }

  public removeProject(name: string) {
    // delete folder
  }

  public buildProject(name: string): IBuildFiddleResponse {
    // shell session for each operation in queue will go to contractPath
    shell.cd(smartContractFolder);
    const contractSub = path.join(this.userPrefix, name);
    const contractName = `${this.userPrefix}_${name}`;
    // buid project
    let execution = shell.exec(
      `RUSTFLAGS='-C link-arg=-s' cargo build -q --release --target wasm32-unknown-unknown -p ${contractName}`,
    );

    if (execution.code !== 0) {
      return {
        // filter leaner message
        message: execution.stderr
          // .replace(/(?:= note|error):.*\n/g, '')
          // .replace(/To learn more.*/, '')
          .trim(),
        success: false,
      };
    }

    // wasm-optimize on all results
    execution = shell.exec(
      `mkdir -p packages/${contractSub}/artifacts && wasm-opt -Os "target/wasm32-unknown-unknown/release/${contractName}.wasm" -o "packages/${contractSub}/artifacts/${name}.wasm"`,
    );
    if (execution.code !== 0) {
      return {
        message: execution.stderr,
        success: false,
      };
    }

    // return true
    return {
      message: execution.stdout,
      success: true,
    };
  }

  public buildSchema(name: string): IBuildFiddleResponse {
    // shell session for each operation in queue will go to contractPath
    shell.cd(smartContractFolder);
    const contractName = `${this.userPrefix}_${name}`;

    // buid project
    let execution = shell.exec(
      `cargo run -q --example schema -p ${contractName}`,
    );

    // just return success or fail message
    if (execution.code !== 0) {
      return {
        message: execution.stderr,
        success: false,
      };
    }

    // return true
    return {
      message: execution.stdout,
      success: true,
    };
  }

  public testProject(name: string): string {
    // shell session for each operation in queue will go to contractPath
    shell.cd(smartContractFolder);
    const contractName = `${this.userPrefix}_${name}`;
    // buid project
    let execution = shell.exec(`cargo test -q --lib -p ${contractName}`);

    // just return success or fail message
    if (execution.code !== 0) {
      return execution.stderr.trim();
    }

    return execution.stdout.trim();
  }
}

import { Injectable } from '@nestjs/common';
import { Request } from 'express';
import path from 'path';
import fs from 'fs';
import fse from 'fs-extra';

import {
  filterName,
  filterPath,
  getFiles,
  getFileSize,
  SmartContractUtils,
} from './app.utils';

const smartContractFolder = process.env.CONTRACT_FOLDER || '/code';
const smartContractPackages = path.join(smartContractFolder, 'packages');
const smartContractUtils = new SmartContractUtils(smartContractFolder);

export interface IFiddleFile {
  name: string;
  data?: string;
  type?: 'binary' | 'text';
}

export interface ISaveFiddleResponse {
  id?: string;
  message: string;
  success: boolean;
}

export interface ILoadFiddleResponse {
  files?: IFiddleFile[];
  id?: string;
  message: string;
  success: boolean;
}

@Injectable()
export class AppService {
  // it run in dist folder
  static readonly clientPath: string = path.join(__dirname);
  async getProject(req: Request): Promise<ILoadFiddleResponse> {
    let { name } = req.query;
    name = filterName(name);
    const contractPath = path.join(smartContractPackages, name);

    if (!fs.existsSync(contractPath)) {
      return {
        success: false,
        message: `Smart Contract ${name} does not exist`,
      };
    } else {
      try {
        const paths = getFiles(contractPath);

        const files = paths.map((value) => {
          const buffer = fs.readFileSync(value);
          return {
            name: value.substring(contractPath.length + 1),
            data: value.endsWith('.wasm')
              ? getFileSize(buffer.length)
              : buffer.toString(),
          };
        });
        return {
          success: true,
          message: 'Success',
          id: name.toString(),
          files,
        };
      } catch (ex) {
        return {
          success: false,
          message: ex.message,
        };
      }
    }
  }

  async saveProject(req: Request): Promise<ISaveFiddleResponse> {
    let { name } = req.query;
    name = filterName(name);
    const { files } = req.body;
    const contractPath = path.join(smartContractPackages, name);

    // TODO: check permission
    const status = fs.existsSync(contractPath) ? 'saved' : 'created';

    try {
      // save all files
      for (let file of files as IFiddleFile[]) {
        await fse.outputFile(
          path.join(contractPath, filterPath(file.name)),
          file.data,
        );
      }

      // add Cargo.toml to project of workspace, so make sure there is file in project to init
      smartContractUtils.initProject(name);

      return {
        id: name.toString(),
        message: `Project ${name} ${status}`,
        success: true,
      };
    } catch (ex) {
      return {
        success: false,
        message: ex.message,
      };
    }
  }

  async saveFile(req: Request): Promise<ISaveFiddleResponse> {
    let { name, data }: IFiddleFile = req.body;
    name = filterPath(name);
    const filePath = path.join(smartContractPackages, name);

    if (filePath.endsWith('Cargo.toml')) {
      return {
        success: false,
        message: `Can not add Cargo.toml for this package: ${name}`,
      };
    }

    const status = fs.existsSync(filePath) ? 'saved' : 'created';

    try {
      // save file
      await fse.outputFile(filePath, data);
      return {
        id: name.toString(),
        message: `File ${name} ${status}`,
        success: true,
      };
    } catch (ex) {
      return {
        success: false,
        message: ex.message,
      };
    }
  }

  async deleteFile(req: Request): Promise<ISaveFiddleResponse> {
    let { name } = req.body;
    name = filterPath(name);
    const filePath = path.join(smartContractPackages, name);

    if (filePath.endsWith('Cargo.toml')) {
      return {
        success: false,
        message: `Can not delete Cargo.toml for this package: ${name}`,
      };
    }

    if (!fs.existsSync(filePath)) {
      return {
        success: false,
        message: `File ${name} does not exist`,
      };
    }

    try {
      // delete file
      await fse.remove(filePath);
      return {
        id: name,
        message: `File ${name} deleted`,
        success: true,
      };
    } catch (ex) {
      return {
        success: false,
        message: ex.message,
      };
    }
  }

  async renameFile(req: Request): Promise<ISaveFiddleResponse> {
    let { name, newName } = req.body;
    name = filterPath(name);
    newName = filterPath(newName);
    const filePath = path.join(smartContractPackages, name);

    if (filePath.endsWith('Cargo.toml')) {
      return {
        success: false,
        message: `Can not rename Cargo.toml for this package: ${name}`,
      };
    }

    if (!fs.existsSync(filePath)) {
      return {
        success: false,
        message: `File ${name} does not exist`,
      };
    }

    try {
      // rename file from the same folder
      const newFilePath = path.join(path.dirname(filePath), newName);
      await fse.rename(filePath, newFilePath);
      return {
        id: newName,
        message: `File ${name} renamed to ${newName}`,
        success: true,
      };
    } catch (ex) {
      return {
        success: false,
        message: ex.message,
      };
    }
  }

  async buildProject(req: Request): Promise<ILoadFiddleResponse> {
    let { name } = req.body;
    name = filterName(name);
    const ret = smartContractUtils.buildProject(name);

    if (!ret.success) {
      return {
        success: false,
        message: ret.message,
      };
    }

    const contractPath = path.join(smartContractPackages, name);
    const fullName = path.join('artifacts', `${name}.wasm`);
    const wasmFilePath = path.join(contractPath, fullName);

    return {
      files: [
        {
          name: fullName,
          data: getFileSize(fs.statSync(wasmFilePath).size),
        },
      ],
      success: true,
      message: 'Build succeeded!',
    };
  }
}

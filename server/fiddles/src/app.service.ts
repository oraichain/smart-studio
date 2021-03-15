import { Injectable } from '@nestjs/common';
import { Request } from 'express';
import path from 'path';
import fs from 'fs';
import fse from 'fs-extra';

import { getFiles, getFileSize, SmartContractUtils } from './app.utils';

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
  getHello(): string {
    return 'Hello World!';
  }

  async getProject(req: Request): Promise<ILoadFiddleResponse> {
    const { name } = req.query;

    const contractPath = path.join(smartContractPackages, name.toString());

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

  async postProject(req: Request): Promise<ISaveFiddleResponse> {
    const { name } = req.query;
    const { files } = req.body;
    const contractPath = path.join(smartContractPackages, name.toString());

    if (fs.existsSync(contractPath)) {
      return {
        success: false,
        message: `Smart Contract ${name} existed`,
      };
    }

    try {
      // save all files
      for (let file of files as IFiddleFile[]) {
        await fse.outputFile(path.join(contractPath, file.name), file.data);
      }

      // add Cargo.toml to project of workspace, so make sure there is file in project to init
      smartContractUtils.initProject(name.toString());

      return {
        id: name.toString(),
        message: `Project ${name} created`,
        success: true,
      };
    } catch (ex) {
      return {
        success: false,
        message: ex.message,
      };
    }
  }

  async postFile(req: Request): Promise<ISaveFiddleResponse> {
    const { name, data }: IFiddleFile = req.body;
    const filePath = path.join(smartContractPackages, name.toString());

    if (filePath.endsWith('Cargo.toml')) {
      return {
        success: false,
        message: `Can not add Cargo.toml for this package: ${name}`,
      };
    }

    if (!fs.existsSync(filePath)) {
      return {
        success: false,
        message: `File ${name} does not exist`,
      };
    }

    try {
      // save file
      await fse.outputFile(filePath, data);
      return {
        id: name.toString(),
        message: `Project ${name} created`,
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
    const { name } = req.body;

    const success = smartContractUtils.buildProject(name);

    if (!success) {
      return {
        success: false,
        message: 'Build failed!',
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

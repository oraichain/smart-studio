import { Injectable } from '@nestjs/common';
import { Request } from 'express';
import path from 'path';
import fs from 'fs';
import fse from 'fs-extra';

import { getFiles, getFileSize } from './app.utils';

const smartContractFolder = process.env.CONTRACT_FOLDER || '/code';

export interface IFiddleFile {
  name: string;
  data?: string;
  type?: 'binary' | 'text';
}

export interface ISaveFiddleResponse {
  id: string;
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

    const contractPath = path.join(smartContractFolder, name.toString());

    if (!fs.existsSync(contractPath)) {
      return {
        success: false,
        message: `Smart Contract ${name} does not existed`,
      };
    } else {
      try {
        const paths = await getFiles(contractPath);

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
    const contractPath = path.join(smartContractFolder, name.toString());

    if (fs.existsSync(contractPath)) {
      return {
        id: null,
        success: false,
        message: `Smart Contract ${name} existed`,
      };
    }

    // save all files
    for (let file of files as IFiddleFile[]) {
      fse.outputFileSync(path.join(contractPath, file.name), file.data);
    }

    return {
      id: name.toString(),
      message: `Project ${name} created`,
      success: true,
    };
  }
}

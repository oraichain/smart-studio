import { Injectable } from '@nestjs/common';
import { Request } from 'express';
import path from 'path';
import fs from 'fs';

import { getFiles, getFileSize } from './app.utils';

const smartContractFolder = process.env.CONTRACT_FOLDER || '/code';

@Injectable()
export class AppService {
  getHello(): string {
    return 'Hello World!';
  }

  async getProject(req: Request): Promise<Object> {
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
          id: name,
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
}

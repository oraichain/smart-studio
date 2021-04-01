import fse from 'fs-extra';
import path from 'path';
import { Inject, Injectable } from '@nestjs/common';
import { PROJECT_DIRECTORY } from './constants';

@Injectable()
export class ProjectService {
  constructor(
    @Inject(PROJECT_DIRECTORY)
    private readonly projectDirectory: string,
  ) {}

  async findAll(username: string) {
    const fullPath = path.join(this.projectDirectory, username);
    const files = fse.readdirSync(fullPath);

    const dirs = await Promise.all(
      files.map(async (s) => {
        if (s === '.DS_Store') {
          return null;
        }

        const st = await fse.lstat(path.join(fullPath, s));
        if (st.isDirectory) {
          return { name: s };
        }

        return null;
      }),
    );

    const pageSize = files.length;

    return {
      items: dirs.filter((s) => !!s),
      pagination: {
        current: 1,
        pageSize, // TODO: parse pageSize and current
      },
    };
  }

  async isExist(projectName: string, user: Express.User) {
    const fullPath = path.join(
      this.projectDirectory,
      user.username,
      projectName,
    );

    if (fse.existsSync(fullPath)) {
      return true;
    }

    return false;
  }
}

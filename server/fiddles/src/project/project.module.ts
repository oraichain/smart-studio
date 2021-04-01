import { DynamicModule, Module } from '@nestjs/common';
import { PROJECT_DIRECTORY } from './constants';
import { ProjectController } from './project.controller';
import { ProjectService } from './project.service';

@Module({})
export class ProjectModule {
  static forRoot(directory: string): DynamicModule {
    return {
      module: ProjectModule,
      controllers: [ProjectController],
      providers: [
        {
          provide: PROJECT_DIRECTORY,
          useValue: directory,
        },
        ProjectService,
      ],
    };
  }
}

import { Controller, Get, Post, Delete, Req, Put } from '@nestjs/common';
import { Request } from 'express';
import {
  AppService,
  ILoadFiddleResponse,
  ISaveFiddleResponse,
} from './app.service';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(): string {
    return this.appService.getHello();
  }

  @Get('project')
  async getProject(@Req() request: Request): Promise<ILoadFiddleResponse> {
    return this.appService.getProject(request);
  }

  @Post('project')
  async postProject(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.postProject(request);
  }

  @Post('file')
  async postFile(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.postFile(request);
  }

  @Put('file')
  async renameFile(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.renameFile(request);
  }

  @Delete('file')
  async deleteFile(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.deleteFile(request);
  }

  @Post('build')
  async buildProject(@Req() request: Request): Promise<any> {
    return this.appService.buildProject(request);
  }
}

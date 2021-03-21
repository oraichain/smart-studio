import path from 'path';
import { Controller, Get, Post, Delete, Req, Put, Res } from '@nestjs/common';
import { Request } from 'express';
import {
  AppService,
  ILoadFiddleResponse,
  ISaveFiddleResponse,
  IFiddleFile,
} from './app.service';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  root(@Res() response): void {
    response.sendFile(path.resolve(AppService.clientPath, 'index.html'));
  }

  @Get('project')
  async getProject(@Req() request: Request): Promise<ILoadFiddleResponse> {
    return this.appService.getProject(request);
  }

  @Post('project')
  async saveProject(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.saveProject(request);
  }

  @Get('file')
  async getFile(@Req() request: Request): Promise<IFiddleFile> {
    return this.appService.getFile(request);
  }

  @Post('file')
  async saveFile(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.saveFile(request);
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
  async buildProject(@Req() request: Request): Promise<ILoadFiddleResponse> {
    return this.appService.buildProject(request);
  }

  @Post('test')
  testProject(@Req() request: Request): string {
    return this.appService.testProject(request);
  }
}

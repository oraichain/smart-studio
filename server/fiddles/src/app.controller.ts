import { Controller, Get, Post, Req } from '@nestjs/common';
import { Request } from 'express';
import { AppService, ILoadFiddleResponse } from './app.service';

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
  async postProject(@Req() request: Request): Promise<any> {
    return this.appService.postProject(request);
  }

  @Post('file')
  async postFile(@Req() request: Request): Promise<any> {
    return this.appService.postFile(request);
  }

  @Post('build')
  async buildProject(@Req() request: Request): Promise<any> {
    return this.appService.buildProject(request);
  }
}

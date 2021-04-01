import path from 'path';
import {
  Controller,
  Get,
  Post,
  Delete,
  Req,
  Put,
  Res,
  UseGuards,
} from '@nestjs/common';
import { Request } from 'express';
import {
  AppService,
  ILoadFiddleResponse,
  ISaveFiddleResponse,
  IFiddleFile,
} from './app.service';
import { JwtAuthGuard } from './auth/jwt.guard';

@Controller()
// @UseGuards(AuthGuard('github'))
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  root(): string {
    return 'Smart Studio Server';
  }

  @Get('project')
  @UseGuards(JwtAuthGuard)
  async getProject(@Req() request: Request): Promise<ILoadFiddleResponse> {
    return this.appService.getProject(request);
  }

  @Post('project')
  @UseGuards(JwtAuthGuard)
  async saveProject(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.saveProject(request);
  }

  @Get('file')
  @UseGuards(JwtAuthGuard)
  async getFile(@Req() request: Request): Promise<IFiddleFile> {
    return this.appService.getFile(request);
  }

  @Post('file')
  @UseGuards(JwtAuthGuard)
  async saveFile(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.saveFile(request);
  }

  @Put('file')
  @UseGuards(JwtAuthGuard)
  async renameFile(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.renameFile(request);
  }

  @Delete('file')
  @UseGuards(JwtAuthGuard)
  async deleteFile(@Req() request: Request): Promise<ISaveFiddleResponse> {
    return this.appService.deleteFile(request);
  }

  @Post('build')
  @UseGuards(JwtAuthGuard)
  buildProject(@Req() request: Request): ILoadFiddleResponse {
    return this.appService.buildProject(request);
  }

  @Post('test')
  @UseGuards(JwtAuthGuard)
  testProject(@Req() request: Request): string {
    return this.appService.testProject(request);
  }

  @Post('schema')
  @UseGuards(JwtAuthGuard)
  buildSchema(@Req() request: Request): ILoadFiddleResponse {
    return this.appService.buildSchema(request);
  }
}

import { JwtAuthGuard } from '@/auth/jwt.guard';
import {
  Body,
  Controller,
  Get,
  Post,
  Query,
  Request,
  UseGuards,
} from '@nestjs/common';
import { ProjectService } from './project.service';

@Controller('projects')
export class ProjectController {
  constructor(private readonly service: ProjectService) {}

  @Get()
  @UseGuards(JwtAuthGuard)
  async list(@Request() req) {
    const result = await this.service.findAll(req.user.username);
    return result;
  }

  @Post('canCreate')
  @UseGuards(JwtAuthGuard)
  async canCreate(@Request() req, @Body('name') projectName: string) {
    const normalizeName = projectName.replace(/(\s\.)/, '');
    if (normalizeName === projectName) {
      const isExist = await this.service.isExist(projectName, req.user);
      if (isExist) {
        return {
          status: false,
          message: 'The project has been created',
        };
      }
    }

    return {
      status: true,
    };
  }
}

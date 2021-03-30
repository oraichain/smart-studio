import { Controller, Get, Query, Request } from '@nestjs/common';
import { ProjectService } from './project.service';

@Controller('projects')
export class ProjectController {
  constructor(private readonly service: ProjectService) {}

  @Get()
  async list(@Query('name') name: string, @Request() req) {
    const result = await this.service.findAll(
      name,
      req.headers.authorization.split(' ')[1],
    );

    return result;
  }
}

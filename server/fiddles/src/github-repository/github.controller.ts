import { Controller, Get, Query, Request } from "@nestjs/common";
import { GithubService } from "./github.service";



@Controller('github')
export class GithubController {

  constructor(
    private readonly service: GithubService
  ) {

  }

  @Get('repositories')
  async list(
    @Query('name') name: string,
    @Request() req,
  ) {
    const result = await this.service.getRepositories(
      name,
      req.headers.authorization.split(' ')[1]
    );

    return result;
  }
}
import { Controller, Get, Request, UseGuards } from "@nestjs/common";
import { AuthGuard } from "@nestjs/passport";


@Controller('auth')
export class GithubController {

  @UseGuards(AuthGuard('github'))
  @Get('github')
  async login(
    @Request() req
  ) {
    return req.user || {};
  }
}
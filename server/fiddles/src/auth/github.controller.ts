import { Controller, Get, Request, UseGuards } from "@nestjs/common";
import { AuthGuard } from "@nestjs/passport";
import { AuthService } from "./auth.service";


@Controller('auth')
export class GithubController {

  constructor(private authService: AuthService) {}
  
  @UseGuards(AuthGuard('github'))
  @Get('github')
  async login(
    @Request() req
  ) {
    return this.authService.login(req.user);
  }
}
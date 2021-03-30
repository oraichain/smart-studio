import { JwtAuthGuard } from "@/auth/jwt.guard";
import { Controller, Get, Req, UseGuards } from "@nestjs/common";



@Controller('profile')
export class ProfileController {

  @Get()
  @UseGuards(JwtAuthGuard)
  async getCurrent(
    @Req() req
  ) {
    return req.user;
  }
}
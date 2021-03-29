import { Injectable } from "@nestjs/common";
import { UsersService } from "@/user/users.service";

@Injectable()
export class AuthService {
  constructor(private usersService: UsersService) {}

  async validateUser(username: string, pass: string): Promise<any> {
    // const user = await this.userService.findOne(username);
    // if (user && user.password === pass) {
    //   const { password, ...result } = user;
    //   return result;
    // }
    return null;
  }
}
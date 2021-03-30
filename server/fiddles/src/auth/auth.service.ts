import { Injectable } from "@nestjs/common";
import { JwtService } from "@nestjs/jwt";

function pick(object, keys) {
  return keys.reduce((obj, key) => {
     if (object && object.hasOwnProperty(key)) {
        obj[key] = object[key];
     }
     return obj;
   }, {});
}
@Injectable()
export class AuthService {
  constructor(
    private jwtService: JwtService
  ) {}

  async login(user: any) {
    const payload = { id: user.id, profile: pick(user, ['id', 'username', 'displayName', 'profileUrl']) };
    return {
      access_token: this.jwtService.sign(payload),
    };
  }
}
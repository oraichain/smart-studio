import { ExtractJwt, Strategy } from 'passport-jwt';
import { PassportStrategy } from '@nestjs/passport';
import { Injectable } from '@nestjs/common';
import { jwtConstants } from './jwt.constants';

@Injectable()
export class JwtStrategy extends PassportStrategy(Strategy) {
  constructor() {
    super({
      jwtFromRequest: ExtractJwt.fromAuthHeaderAsBearerToken(),
      ignoreExpiration: false,
      secretOrKey: jwtConstants.secret,
    });
  }

  async validate(payload: any) {
    //   id: '1710072',
    // displayName: 'Vũ Quang Thịnh',
    // username: 'vuquangthinh',
    // profileUrl: 'https://github.com/vuquangthinh',

    return {
      id: payload.sub,
      ...payload.profile,
    };
  }
}

import { Strategy } from 'passport-github';
import { PassportStrategy } from '@nestjs/passport';
import { Inject, Injectable, UnauthorizedException } from '@nestjs/common';
import { AuthService } from './auth.service';
import { GITHUB_CONFIG } from './constants';

@Injectable()
export class GithubStrategy extends PassportStrategy(Strategy) {
  constructor(
    @Inject(GITHUB_CONFIG) config,
    private authService: AuthService) {
    super(config);
  }

  async validate(accessToken, _, user): Promise<any> {
    // const user = await this.authService.validateUser(username, password);
    if (!user) {
      throw new UnauthorizedException();
    }

    return {
      ...user,
      access_token: accessToken
    };
  }
}
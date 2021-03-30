import { Module } from '@nestjs/common';
import { PassportModule } from '@nestjs/passport';
import { UserModule } from '@/user/user.module';
import { AuthService } from './auth.service';
import { GithubStrategy } from './github.strategy';
import { GITHUB_CONFIG } from './constants';
import { GithubController } from './github.controller';
import { JwtModule } from '@nestjs/jwt';
import { jwtConstants } from './jwt.constants';
import { JwtStrategy } from './jwt.strategy';

@Module({
  imports: [
    UserModule,
    PassportModule,
    JwtModule.register({
      secret: jwtConstants.secret,
      signOptions: { expiresIn: 86400 * 7 },
    }),
  ],
  providers: [
    AuthService,
    GithubStrategy,
    JwtStrategy,
    {
      provide: GITHUB_CONFIG,
      useValue: {
        clientID: process.env.GITHUB_CLIENT_ID || 'c374bfc1b298588a0f2a',
        clientSecret:
          process.env.GITHUB_CLIENT_SECRET ||
          '92f9ca21378ae1a4bd221ba45745ec8b923c6c99',
        callbackURL: process.env.GITHUB_CALLBACK_URL || '/',
      },
    },
  ],

  controllers: [GithubController],
})
export class AuthModule {}

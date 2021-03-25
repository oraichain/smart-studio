import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { ServeStaticModule } from '@nestjs/serve-static';
import { AuthModule } from './auth/auth.module';
import { UserModule } from './user/user.module';
import { GithubRepositoryModule } from './github-repository/github-repository.module';

@Module({
  imports: [
    AuthModule,
    UserModule,
    GithubRepositoryModule,
    ServeStaticModule.forRoot({
      rootPath: AppService.clientPath,
    }),
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}

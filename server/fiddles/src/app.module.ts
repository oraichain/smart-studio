import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
// import { ServeStaticModule } from '@nestjs/serve-static';
import { AuthModule } from './auth/auth.module';
import { UserModule } from './user/user.module';
import { ProjectModule } from './project/project.module';
import { smartContractPackages } from './app.utils';

@Module({
  imports: [
    AuthModule,
    UserModule,
    ProjectModule.forRoot(smartContractPackages),
    // ServeStaticModule.forRoot({
    //   rootPath: AppService.clientPath,
    // }),
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}

import { Module } from '@nestjs/common';
import path from 'path';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { ServeStaticModule } from '@nestjs/serve-static';

@Module({
  imports: [
    ServeStaticModule.forRoot({
      rootPath: AppService.clientPath,
    }),
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}

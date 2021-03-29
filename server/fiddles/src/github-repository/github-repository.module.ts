import { Module } from "@nestjs/common";
import { GithubController } from "./github.controller";
import { GithubService } from "./github.service";


@Module({
  controllers: [
    GithubController
  ],
  providers: [
    GithubService
  ]
})
export class GithubRepositoryModule {
}
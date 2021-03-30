import { Module } from "@nestjs/common";
import { ProfileController } from "./profile.controller";


@Module({
  providers: [
  ],
  exports: [
  ],
  controllers: [
    ProfileController,
  ]
})
export class UserModule {

}
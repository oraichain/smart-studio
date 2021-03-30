import { Injectable } from "@nestjs/common";


@Injectable()
export class ProjectService {
  async findAll(
    name: string,
    token: string,
    current: number = 1,
    pageSize: number = 10
  ) {
    return {
      items: [],
      pagination: {
        current,
        pageSize  // TODO: parse pageSize and current
      }
    };
  }
}
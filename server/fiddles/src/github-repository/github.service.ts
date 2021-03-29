import { Injectable } from "@nestjs/common";
import axios, { AxiosInstance } from 'axios'

axios

@Injectable()
export class GithubService {

  client: AxiosInstance;

  constructor() {
    this.client = axios.create({
      baseURL: 'https://api.github.com'
    });
  }

  async getRepositories(
    name: string,
    token: string,
    current: number = 1,
    pageSize: number = 10
  ) {

    const response = await this.client.get(`/users/${name}/repos`, {
      params: {
        page: current,
        pageSize
      },
      headers: {
        Authorization: `Bearer ${token}`
      },
    });

    return {
      items: response.data,
      pagination: {
        current,
        pageSize  // TODO: parse pageSize and current
      }
    };
  }
}
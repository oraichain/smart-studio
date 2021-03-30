declare global {
  namespace Express {
    interface User {
      id: string;
      username: string;
      displayName: string;
      profileUrl: string;
    }
  }
}

export {};

declare global {
  interface Window {
    MonacoEnvironment: {
      getWorkerUrl(): string;
    };
  }

  namespace NodeJS {
    interface ProcessEnv {
      SERVICE_URL: string;
      NODE_ENV: 'production' | 'development';
    }
  }
}

export {};

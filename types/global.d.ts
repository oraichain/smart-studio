declare global {
  interface Window {
    MonacoEnvironment: {
      getWorkerUrl(workerId: string, label: string): string;
    };
  }

  namespace NodeJS {
    interface ProcessEnv {
      SERVICE_URL: string;
      NODE_ENV: 'production' | 'development';
    }
  }
}

export { };

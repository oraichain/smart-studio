declare global {
  interface Window {
    MonacoEnvironment: {
      getWorkerUrl(): string;
    };
  }

  namespace NodeJS {
    interface ProcessEnv {
      SERVICE_URL: string;
    }
  }
}

export {};

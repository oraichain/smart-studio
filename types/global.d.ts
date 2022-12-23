import { eddsa as ellipticEddsa } from 'elliptic';

declare global {
  var _eddsa: ellipticEddsa; // we use a global to prevent serialization issues for the calling class
  function eddsa(): ellipticEddsa;

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

export {};

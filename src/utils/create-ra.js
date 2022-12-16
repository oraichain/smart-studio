// Create an RA Web worker
export const createRA = async () => {
  const worker = new Worker(new URL('../ra-worker.js', import.meta.url));
  const pendingResolve = {};

  let id = 1;
  let ready;

  const callWorker = async (which, ...args) => {
    return new Promise((resolve, _) => {
      pendingResolve[id] = resolve;
      worker.postMessage({
        which: which,
        args: args,
        id: id
      });
      id += 1;
    });
  };

  const proxyHandler = {
    get: (target, prop, _receiver) => {
      if (prop == 'then') {
        return Reflect.get(target, prop, _receiver);
      }
      return async (...args) => {
        return callWorker(prop, ...args);
      };
    }
  };

  worker.onmessage = (e) => {
    if (e.data.id == 'ra-worker-ready') {
      ready(new Proxy({}, proxyHandler));
      return;
    }
    const pending = pendingResolve[e.data.id];
    if (pending) {
      pending(e.data.result);
      delete pendingResolve[e.data.id];
    }
  };

  return new Promise((resolve, _) => {
    ready = resolve;
  });
};

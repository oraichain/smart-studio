import init, { initThreadPool, WorldState } from '../crates/ra-wasm/pkg/ra_wasm';

const start = async () => {
  await init();

  // Thread pool initialization with the given number of threads
  // (pass `navigator.hardwareConcurrency` if you want to use all cores).
  // console.log(navigator.hardwareConcurrency);
  // use single thread for faster communication
  await initThreadPool(1);

  const state = new WorldState();

  onmessage = (e) => {
    const { which, args, id } = e.data;

    const result = state[which](...args);

    postMessage({
      id: id,
      result: result
    });
  };
};

start().then(() => {
  postMessage({
    id: 'ra-worker-ready'
  });
});

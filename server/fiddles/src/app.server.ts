import express, { Request, Response } from 'express';
import expressWs from 'express-ws';
import path from 'path';
import fs from 'fs';
import os from 'os';
import * as pty from 'node-pty';
import cors from 'cors';

import { smartContractPackages, filterName } from './app.utils';

const USE_BINARY = os.platform() !== 'win32';
// after 60s no request then timeout, must re-connect
const maxIdleTimeout = 60000;

const server = expressWs(express()).app;
server.use(cors());

export interface ITerminals {
  [details: string]: pty.IPty;
}

const terminals: ITerminals = {};
const logs = {};

// string message buffering
function buffer(socket, timeout) {
  let s = '';
  let sender = null;
  return (data) => {
    s += data;
    if (!sender) {
      sender = setTimeout(() => {
        socket.send(s);
        s = '';
        sender = null;
      }, timeout);
    }
  };
}
// binary message buffering
function bufferUtf8(socket, timeout) {
  let buffer = [];
  let sender = null;
  let length = 0;
  return (data) => {
    buffer.push(data);
    length += data.length;
    if (!sender) {
      sender = setTimeout(() => {
        socket.send(Buffer.concat(buffer, length));
        buffer = [];
        sender = null;
        length = 0;
      }, timeout);
    }
  };
}

server.post('/terminals', (req: Request, res: Response) => {
  const env = Object.assign({}, process.env);
  env['COLORTERM'] = 'truecolor';
  const name = filterName(req.query.name.toString());

  const contractPath = path.join(smartContractPackages, name);
  const fullName = path.join('artifacts', `${name}.wasm`);
  const wasmFile = path.join(contractPath, fullName);

  if (!fs.existsSync(wasmFile)) {
    res.end();
    return;
  }

  const cols = parseInt(req.query.cols.toString()),
    rows = parseInt(req.query.rows.toString()),
    term = pty.spawn('/usr/bin/simulate', [wasmFile], {
      name: 'xterm-256color',
      cols: cols || 80,
      rows: rows || 24,
      cwd: process.platform === 'win32' ? undefined : env.PWD,
      env: env,
      encoding: USE_BINARY ? null : 'utf8',
    });

  console.log('Created terminal with PID: ' + term.pid);
  terminals[term.pid] = term;
  logs[term.pid] = '';
  term.on('data', function (data) {
    logs[term.pid] += data;
  });
  res.send(term.pid.toString());
  res.end();
});

server.post('/terminals/:pid/size', (req: Request, res: Response) => {
  const pid = parseInt(req.params.pid),
    cols = parseInt(req.query.cols.toString()),
    rows = parseInt(req.query.rows.toString()),
    term = terminals[pid];

  if (term) {
    try {
      term.resize(cols, rows);
    } catch {}
    console.log(
      'Resized terminal ' +
        pid +
        ' to ' +
        cols +
        ' cols and ' +
        rows +
        ' rows.',
    );
  }
  res.end();
});

server.ws('/terminals/:pid', function (ws, req) {
  const term = terminals[parseInt(req.params.pid)];
  if (!term) {
    ws.close();
    return;
  }
  console.log('Connected to terminal ' + term.pid);
  ws.send(logs[term.pid]);

  const send = USE_BINARY ? bufferUtf8(ws, 5) : buffer(ws, 5);

  term.onData((data) => {
    try {
      send(data);
    } catch (ex) {
      // The WebSocket is not open, ignore
    }
  });
  const closeSocketFn = () => {
    ws.terminate();
  };
  let timer: NodeJS.Timeout = setTimeout(closeSocketFn, maxIdleTimeout);
  ws.on('message', (msg: any) => {
    clearTimeout(timer);
    term.write(msg);
    timer = setTimeout(closeSocketFn, maxIdleTimeout);
  });
  ws.on('close', () => {
    term.kill();
    console.log('Closed terminal ' + term.pid);
    // Clean things up
    delete terminals[term.pid];
    delete logs[term.pid];
  });
});

export { server };

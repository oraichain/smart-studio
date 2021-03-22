/* Copyright 2018 Mozilla Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
import { File, Project, Directory, FileType, Problem, isBinaryFileType, fileTypeFromFileName, IStatusProvider } from './models';
import { padLeft, padRight, isBranch, toAddress, decodeRestrictedBase64ToBytes, base64EncodeBytes } from './util';
import { WorkerCommand, IWorkerResponse } from './message';
import { processJSFile, RewriteSourcesContext } from './utils/rewriteSources';
import { getCurrentRunnerInfo } from './utils/taskRunner';
import { createCompilerService, Language } from './compilerServices';
import { getServiceURL, ServiceTypes } from './compilerServices/sendRequest';
import { file } from 'jszip';

declare var capstone: {
  ARCH_X86: any;
  MODE_64: any;
  Cs: any;
};

declare var Module: ({}) => any;

declare var showdown: {
  Converter: any;
  setFlavor: Function;
};

export interface IFiddleFile {
  name: string;
  data?: string;
  type?: 'binary' | 'text';
}

export interface ISaveFiddleResponse {
  id: string;
  message: string;
  success: boolean;
}

export interface ICreateFiddleRequest {
  files: IFiddleFile[];
}

export interface ILoadFiddleResponse {
  files: IFiddleFile[];
  id: string;
  message: string;
  success: boolean;
}

export { Language } from './compilerServices';

function getProjectFilePath(file: File): string {
  const project = file.getProject();
  return file.getPath(project);
}

export class Service {
  // private static worker = new ServiceWorker();

  static getMarkers(response: string): monaco.editor.IMarkerData[] {
    // Parse and annotate errors if compilation fails.
    const annotations: monaco.editor.IMarkerData[] = [];
    if (response.indexOf('(module') !== 0) {
      const re1 = /^.*?:(\d+?):(\d+?):\s(.*)$/gm;
      let m: any;
      // Single position.
      while ((m = re1.exec(response)) !== null) {
        if (m.index === re1.lastIndex) {
          re1.lastIndex++;
        }
        const startLineNumber = parseInt(m[1], 10);
        const startColumn = parseInt(m[2], 10);
        const message = m[3];
        let severity = monaco.MarkerSeverity.Info;
        if (message.indexOf('error') >= 0) {
          severity = monaco.MarkerSeverity.Error;
        } else if (message.indexOf('warning') >= 0) {
          severity = monaco.MarkerSeverity.Warning;
        }
        annotations.push({
          severity,
          message,
          startLineNumber: startLineNumber,
          startColumn: startColumn,
          endLineNumber: startLineNumber,
          endColumn: startColumn
        });
      }
      // Range. This is generated via the -diagnostics-print-source-range-info
      // clang flag.
      const re2 = /^.*?:\d+?:\d+?:\{(\d+?):(\d+?)-(\d+?):(\d+?)\}:\s(.*)$/gm;
      while ((m = re2.exec(response)) !== null) {
        if (m.index === re2.lastIndex) {
          re2.lastIndex++;
        }
        const message = m[5];
        let severity = monaco.MarkerSeverity.Info;
        if (message.indexOf('error') >= 0) {
          severity = monaco.MarkerSeverity.Error;
        } else if (message.indexOf('warning') >= 0) {
          severity = monaco.MarkerSeverity.Warning;
        }
        annotations.push({
          severity,
          message,
          startLineNumber: parseInt(m[1], 10),
          startColumn: parseInt(m[2], 10),
          endLineNumber: parseInt(m[3], 10),
          endColumn: parseInt(m[4], 10)
        });
      }
    }
    return annotations;
  }

  static async compileFiles(files: File[], from: Language, to: Language, options = ''): Promise<{ [name: string]: string | ArrayBuffer }> {
    const service = await createCompilerService(from, to);

    const fileNameMap: { [name: string]: File } = files.reduce(
      (acc: any, f: File) => {
        acc[getProjectFilePath(f)] = f;
        return acc;
      },
      {} as any
    );

    const input = {
      files: files.reduce(
        (acc: any, f: File) => {
          acc[getProjectFilePath(f)] = {
            content: f.getData()
          };
          return acc;
        },
        {} as any
      ),
      options
    };
    const result = await service.compile(input);

    for (const file of files) {
      file.setProblems([]);
    }

    for (const [name, item] of Object.entries(result.items)) {
      const { fileRef, console } = item;
      if (!fileRef || !console) {
        continue;
      }
      const file = fileNameMap[fileRef];
      if (!file) {
        continue;
      }
      const markers = Service.getMarkers(console);
      if (markers.length > 0) {
        monaco.editor.setModelMarkers(file.buffer, 'compiler', markers);
        file.setProblems(
          markers.map((marker) => {
            return Problem.fromMarker(file, marker);
          })
        );
      }
    }

    if (!result.success) {
      throw new Error(result.console);
    }

    const outputFiles: any = {};
    for (const [name, item] of Object.entries(result.items)) {
      const { content } = item;
      if (content) {
        outputFiles[name] = content;
      }
    }
    return outputFiles;
  }

  static async compileFile(file: File, from: Language, to: Language, options = ''): Promise<any> {
    const result = await Service.compileFileWithBindings(file, from, to, options);
    return result.wasm;
  }

  static async compileFileWithBindings(file: File, from: Language, to: Language, options = ''): Promise<any> {
    if (to !== Language.Wasm) {
      throw new Error(`Only wasm target is supported, but "${to}" was found`);
    }
    const result = await Service.compileFiles([file], from, to, options);
    const expectedOutputFilename = 'a.wasm';
    let output: any = {
      wasm: result[expectedOutputFilename]
    };
    const expectedWasmBindgenJsFilename = 'wasm_bindgen.js';
    if (result[expectedWasmBindgenJsFilename]) {
      output = {
        ...output,
        wasmBindgenJs: result[expectedWasmBindgenJsFilename]
      };
    }
    return output;
  }

  static async loadJSON(uri: string): Promise<ILoadFiddleResponse> {
    const baseURL = await getServiceURL(ServiceTypes.Service);
    const response = await fetch(`${baseURL}/project?name=${uri}`, {
      headers: new Headers({ 'Content-type': 'application/json; charset=utf-8' })
    });
    return await response.json();
  }

  static async saveJSON(json: ICreateFiddleRequest, uri: string): Promise<ISaveFiddleResponse> {
    const update = !!uri;
    if (update) {
      const baseURL = await getServiceURL(ServiceTypes.Service);
      const response = await fetch(`${baseURL}/project?name=${uri}`, {
        method: 'POST',
        headers: new Headers({ 'Content-type': 'application/json; charset=utf-8' }),
        body: JSON.stringify(json)
      });
      const ret = await response.json();
      return ret;
    }
  }

  static async saveFile(file: File): Promise<ISaveFiddleResponse> {
    const fileData = file.getData();
    // there is no content to save
    if (!fileData) {
      return {
        id: file.name,
        success: true,
        message: `File ${file.name} created`
      };
    }

    const json: IFiddleFile = {
      name: file.getPath(),
      data: fileData.toString()
    };
    const baseURL = await getServiceURL(ServiceTypes.Service);
    const response = await fetch(`${baseURL}/file`, {
      method: 'POST',
      headers: new Headers({ 'Content-type': 'application/json; charset=utf-8' }),
      body: JSON.stringify(json)
    });
    const ret = await response.json();
    return ret;
  }

  static async deleteFile(file: File): Promise<ISaveFiddleResponse> {
    const json: IFiddleFile = {
      name: file.getPath()
    };
    const baseURL = await getServiceURL(ServiceTypes.Service);
    const response = await fetch(`${baseURL}/file`, {
      method: 'DELETE',
      headers: new Headers({ 'Content-type': 'application/json; charset=utf-8' }),
      body: JSON.stringify(json)
    });
    const ret = await response.json();
    return ret;
  }

  static async renameFile(file: File, name: string): Promise<ISaveFiddleResponse> {
    const json = {
      name: file.getPath(),
      newName: name
    };
    const baseURL = await getServiceURL(ServiceTypes.Service);
    const response = await fetch(`${baseURL}/file`, {
      method: 'PUT',
      headers: new Headers({ 'Content-type': 'application/json; charset=utf-8' }),
      body: JSON.stringify(json)
    });
    const ret = await response.json();
    return ret;
  }

  static parseFiddleURI(): string {
    let uri = window.location.search.substring(1);
    if (uri) {
      const i = uri.indexOf('/');
      if (i > 0) {
        uri = uri.substring(0, i);
      }
    }
    return uri;
  }

  static async exportToWallet(file: File): Promise<IFiddleFile> {
    const filePath = file.getPath();
    const baseURL = await getServiceURL(ServiceTypes.Service);
    const response = await fetch(`${baseURL}/file?name=${filePath}`);
    const ret = await response.json();
    return ret;
  }

  static async buildProject(name: string): Promise<ILoadFiddleResponse> {
    const baseURL = await getServiceURL(ServiceTypes.Service);
    const response = await fetch(`${baseURL}/build`, {
      method: 'POST',
      headers: new Headers({ 'Content-type': 'application/json; charset=utf-8' }),
      body: JSON.stringify({ name })
    });
    const ret = await response.json();
    return ret;
  }

  static async buildSchema(name: string): Promise<ILoadFiddleResponse> {
    const baseURL = await getServiceURL(ServiceTypes.Service);
    const response = await fetch(`${baseURL}/schema`, {
      method: 'POST',
      headers: new Headers({ 'Content-type': 'application/json; charset=utf-8' }),
      body: JSON.stringify({ name })
    });
    const ret = await response.json();
    return ret;
  }

  // TODO: wrap with handler post, get and show error
  static async testProject(name: string): Promise<string> {
    const baseURL = await getServiceURL(ServiceTypes.Service);
    const response = await fetch(`${baseURL}/test`, {
      method: 'POST',
      headers: new Headers({ 'Content-type': 'application/json; charset=utf-8' }),
      body: JSON.stringify({ name })
    });
    const message = await response.text();
    return message;
  }

  static async saveProject(project: Project, uploadedFiles?: File[]): Promise<ISaveFiddleResponse> {
    const files: IFiddleFile[] = [];
    // if there is no upload files, save current projects, otherwise save new uploadedFiles
    const processFile = (f: File) => {
      let data: string;
      let type: 'binary' | 'text';
      if (isBinaryFileType(f.type)) {
        data = base64EncodeBytes(new Uint8Array(f.data as ArrayBuffer));
        type = 'binary';
      } else {
        data = f.data as string;
        type = 'text';
      }
      const file = {
        name: f.getPath(project),
        data,
        type
      };
      files.push(file);
    };

    if (!Array.isArray(uploadedFiles)) {
      project.forEachFile(processFile, true, true);
    } else {
      // if there is directory, do loop
      for (let f of uploadedFiles) {
        if (f.type === FileType.Directory) {
          (f as Directory).forEachFile(processFile, true, true);
        } else {
          processFile(f);
        }
      }
    }

    return await this.saveJSON(
      {
        files
      },
      project.name
    );
  }

  static async loadFilesIntoProject(files: IFiddleFile[], project: Project, base: URL = null): Promise<any> {
    for (const f of files) {
      const type = fileTypeFromFileName(f.name);
      const file = project.newFile(f.name, type, false);
      let data: string | ArrayBuffer;
      if (f.data) {
        if (f.type === 'binary') {
          data = decodeRestrictedBase64ToBytes(f.data).buffer as ArrayBuffer;
        } else {
          data = f.data;
        }
      } else {
        const request = await fetch(new URL(f.name, base).toString());
        if (f.type === 'binary') {
          data = await request.arrayBuffer();
        } else {
          data = await request.text();
        }
      }
      file.setData(data);
    }
  }

  static lazyLoad(uri: string, status?: IStatusProvider): Promise<any> {
    return new Promise((resolve, reject) => {
      status && status.push('Loading ' + uri);
      const self = this;
      const d = window.document;
      const b = d.body;
      const e = d.createElement('script');
      e.async = true;
      e.src = uri;
      b.appendChild(e);
      e.onload = function() {
        status && status.pop();
        resolve(this);
      };
      // TODO: What about fail?
    });
  }

  static downloadLink: HTMLAnchorElement = null;
  static download(file: File) {
    if (!Service.downloadLink) {
      Service.downloadLink = document.createElement('a');
      Service.downloadLink.style.display = 'none';
      document.body.appendChild(Service.downloadLink);
    }

    const blobPart = isBinaryFileType(file.type) ? Buffer.from(file.getData()) : file.getData();
    // download binary with buffer otherwise using string
    const url = URL.createObjectURL(new Blob([blobPart], { type: 'application/octet-stream' }));
    Service.downloadLink.href = url;
    Service.downloadLink.download = file.name;
    if ((Service.downloadLink.href as any) !== document.location) {
      Service.downloadLink.click();
    }
  }

  static clangFormatModule: any = null;
  // Kudos to https://github.com/tbfleming/cib
  static async clangFormat(file: File, status?: IStatusProvider) {
    function format() {
      const result = Service.clangFormatModule.ccall('formatCode', 'string', ['string'], [file.buffer.getValue()]);
      file.buffer.setValue(result);
    }

    if (Service.clangFormatModule) {
      format();
    } else {
      await Service.lazyLoad('lib/clang-format.js', status);
      const response = await fetch('lib/clang-format.wasm');
      const wasmBinary = await response.arrayBuffer();
      const module: any = {
        postRun() {
          format();
        },
        wasmBinary
      };
      Service.clangFormatModule = Module(module);
    }
  }

  static async disassembleX86(file: File, status?: IStatusProvider, options = '') {
    if (typeof capstone === 'undefined') {
      await Service.lazyLoad('lib/capstone.x86.min.js', status);
    }
    const output = file.parent.newFile(file.name + '.x86', FileType.x86);

    function toBytes(a: any) {
      return a
        .map(function(x: any) {
          return padLeft(Number(x).toString(16), 2, '0');
        })
        .join(' ');
    }

    const service = await createCompilerService(Language.Wasm, Language.x86);
    const input = {
      files: {
        'in.wasm': {
          content: file.getData()
        }
      },
      options
    };
    const result = await service.compile(input);
    const json: any = result.items['a.json'].content;
    let s = '';
    const cs = new capstone.Cs(capstone.ARCH_X86, capstone.MODE_64);
    const annotations: any[] = [];
    const assemblyInstructionsByAddress = Object.create(null);
    for (let i = 0; i < json.regions.length; i++) {
      const region = json.regions[i];
      s += region.name + ':\n';
      const csBuffer = decodeRestrictedBase64ToBytes(region.bytes);
      const instructions = cs.disasm(csBuffer, region.entry);
      const basicBlocks: any = {};
      instructions.forEach(function(instr: any, i: any) {
        assemblyInstructionsByAddress[instr.address] = instr;
        if (isBranch(instr)) {
          const targetAddress = parseInt(instr.op_str, 10);
          if (!basicBlocks[targetAddress]) {
            basicBlocks[targetAddress] = [];
          }
          basicBlocks[targetAddress].push(instr.address);
          if (i + 1 < instructions.length) {
            basicBlocks[instructions[i + 1].address] = [];
          }
        }
      });
      instructions.forEach(function(instr: any) {
        if (basicBlocks[instr.address]) {
          s += ' ' + padRight(toAddress(instr.address) + ':', 39, ' ');
          if (basicBlocks[instr.address].length > 0) {
            s += '; ' + toAddress(instr.address) + ' from: [' + basicBlocks[instr.address].map(toAddress).join(', ') + ']';
          }
          s += '\n';
        }
        s += '  ' + padRight(instr.mnemonic + ' ' + instr.op_str, 38, ' ');
        s += '; ' + toAddress(instr.address) + ' ' + toBytes(instr.bytes) + '\n';
      });
      s += '\n';
    }
    output.setData(s);
  }

  private static binaryExplorerMessageListener: (e: any) => void;

  static openBinaryExplorer(file: File) {
    window.open(
      '//wasdk.github.io/wasmcodeexplorer/?api=postmessage',
      '',
      'toolbar=no,ocation=no,directories=no,status=no,menubar=no,location=no,scrollbars=yes,resizable=yes,width=1024,height=568'
    );
    if (Service.binaryExplorerMessageListener) {
      window.removeEventListener('message', Service.binaryExplorerMessageListener, false);
    }
    Service.binaryExplorerMessageListener = (e: any) => {
      if (e.data.type === 'wasmexplorer-ready') {
        window.removeEventListener('message', Service.binaryExplorerMessageListener, false);
        Service.binaryExplorerMessageListener = null;
        const dataToSend = new Uint8Array((file.data as any).slice(0));
        e.source.postMessage(
          {
            type: 'wasmexplorer-load',
            data: dataToSend
          },
          '*',
          [dataToSend.buffer]
        );
      }
    };
    window.addEventListener('message', Service.binaryExplorerMessageListener, false);
  }

  static async import(path: string): Promise<any> {
    const { project, global } = getCurrentRunnerInfo();
    const context = new RewriteSourcesContext(project);
    context.logLn = console.log;
    context.createFile = (src: ArrayBuffer | string, type: string) => {
      const blob = new global.Blob([src], { type });
      return global.URL.createObjectURL(blob);
    };

    const url = processJSFile(context, path);
    // Create script tag to load ES module.
    const script = global.document.createElement('script');
    script.setAttribute('type', 'module');
    script.setAttribute('async', 'async');
    const id = `__import__${Math.random()
      .toString(36)
      .substr(2)}`;
    const scriptReady = new Promise((resolve, reject) => {
      global[id] = resolve;
    });
    script.textContent = `import * as i from '${url}'; ${id}(i);`;
    global.document.head.appendChild(script);
    const module = await scriptReady;
    // Module loaded -- cleaning up
    script.remove();
    delete global[id];
    return module;
  }

  static async compileMarkdownToHtml(src: string): Promise<string> {
    if (typeof showdown === 'undefined') {
      await Service.lazyLoad('lib/showdown.min.js');
    }
    const converter = new showdown.Converter({ tables: true, ghCodeBlocks: true });
    showdown.setFlavor('github');
    return converter.makeHtml(src);
  }
}

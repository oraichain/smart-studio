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

import { logKind } from '../actions/AppActions';
import { Project } from './Project';

export enum FileType {
  JavaScript = 'javascript',
  TypeScript = 'typescript',
  HTML = 'html',
  CSS = 'css',
  C = 'c',
  Cpp = 'cpp',
  Rust = 'rust',
  Wat = 'wat',
  Wasm = 'wasm',
  Directory = 'directory',
  Log = 'log',
  x86 = 'x86',
  Markdown = 'markdown',
  Cretonne = 'cretonne',
  JSON = 'json',
  DOT = 'dot',
  TOML = 'toml',
  Unknown = 'unknown'
}

export interface SandboxRun {
  project: Project;
  src: string;
}

export interface IStatusProvider {
  push(status: string): void;
  pop(): void;
  logLn(message: string, kind?: logKind): void;
}

export function isBinaryFileType(type: FileType) {
  switch (type) {
    case FileType.Wasm:
      return true;
    default:
      return false;
  }
}

export function languageForFileType(type: FileType): string {
  switch (type) {
    case FileType.HTML:
      return 'html';
    case FileType.CSS:
      return 'css';
    case FileType.JavaScript:
      return 'javascript';
    case FileType.TypeScript:
      return 'typescript';
    case FileType.C:
    case FileType.Cpp:
      return 'cpp';
    case FileType.Rust:
      return 'rust';
    case FileType.Wat:
    case FileType.Wasm:
      return 'wat';
    case FileType.Log:
      return 'log';
    case FileType.x86:
      return 'x86';
    case FileType.Markdown:
      return 'markdown';
    case FileType.Cretonne:
      return 'cton';
    case FileType.JSON:
      return 'json';
    case FileType.DOT:
      return 'dot';
    case FileType.TOML:
      return 'toml';
    default:
      return '';
  }
}

export function nameForFileType(type: FileType): string {
  switch (type) {
    case FileType.HTML:
      return 'HTML';
    case FileType.CSS:
      return 'CSS';
    case FileType.JavaScript:
      return 'JavaScript';
    case FileType.TypeScript:
      return 'TypeScript';
    case FileType.C:
      return 'C';
    case FileType.Cpp:
      return 'C++';
    case FileType.Wat:
      return 'WebAssembly Text';
    case FileType.Wasm:
      return 'WebAssembly';
    case FileType.Markdown:
      return 'Markdown';
    case FileType.Rust:
      return 'Rust';
    case FileType.Cretonne:
      return 'Cretonne';
    case FileType.JSON:
      return 'JSON';
    case FileType.DOT:
      return 'DOT';
    case FileType.TOML:
      return 'TOML';
    default:
      return '';
  }
}

export function extensionForFileType(type: FileType): string {
  switch (type) {
    case FileType.HTML:
      return 'html';
    case FileType.CSS:
      return 'css';
    case FileType.JavaScript:
      return 'js';
    case FileType.TypeScript:
      return 'ts';
    case FileType.C:
      return 'c';
    case FileType.Cpp:
      return 'cpp';
    case FileType.Wat:
      return 'wat';
    case FileType.Wasm:
      return 'wasm';
    case FileType.Markdown:
      return 'md';
    case FileType.Rust:
      return 'rs';
    case FileType.Cretonne:
      return 'cton';
    case FileType.JSON:
      return 'json';
    case FileType.DOT:
      return 'dot';
    case FileType.TOML:
      return 'toml';
    default:
      return '';
  }
}

export function fileTypeFromFileName(name: string): FileType {
  return fileTypeForExtension(name.split('.').pop());
}

export function fileTypeForExtension(extension: string): FileType {
  switch (extension) {
    case 'html':
      return FileType.HTML;
    case 'css':
      return FileType.CSS;
    case 'js':
      return FileType.JavaScript;
    case 'ts':
      return FileType.TypeScript;
    case 'c':
      return FileType.C;
    case 'cpp':
      return FileType.Cpp;
    case 'wat':
      return FileType.Wat;
    case 'wasm':
      return FileType.Wasm;
    case 'md':
      return FileType.Markdown;
    case 'rs':
      return FileType.Rust;
    case 'cton':
      return FileType.Cretonne;
    case 'json' || extension === 'map':
      return FileType.JSON;
    case 'dot':
      return FileType.DOT;
    case 'toml':
      return FileType.TOML;
    default:
      return null;
  }
}

export function mimeTypeForFileType(type: FileType): string {
  switch (type) {
    case FileType.HTML:
      return 'text/html';
    case FileType.JavaScript:
      return 'application/javascript';
    case FileType.Wasm:
      return 'application/wasm';
    case FileType.JSON:
      return 'application/json';
    case FileType.DOT:
      return 'text/plain';
    case FileType.Markdown:
      return 'text/markdown';
    default:
      return '';
  }
}

export function fileTypeForMimeType(type: string): FileType {
  switch (type) {
    case 'text/html':
      return FileType.HTML;
    case 'application/javascript':
      return FileType.JavaScript;
    case 'application/wasm':
      return FileType.Wasm;
    case 'text/markdown':
      return FileType.Markdown;
    case 'application/json':
      return FileType.JSON;
    default:
      return FileType.Unknown;
  }
}

export function getIconForFileType(fileType: FileType): string {
  switch (fileType) {
    case FileType.JavaScript:
      return 'javascript-lang-file-icon';
    case FileType.TypeScript:
      return 'typescript-lang-file-icon';
    case FileType.C:
      return 'c-lang-file-icon';
    case FileType.Cpp:
      return 'cpp-lang-file-icon';
    case FileType.Rust:
      return 'rust-lang-file-icon';
    case FileType.Markdown:
      return 'markdown-lang-file-icon';
    case FileType.HTML:
      return 'html-lang-file-icon';
    case FileType.CSS:
      return 'css-lang-file-icon';
    case FileType.Directory:
      return 'folder-icon';
    case FileType.JSON:
      return 'json-lang-file-icon';
    case FileType.Wasm:
      return 'wasm-lang-file-icon';
    case FileType.Wat:
      return 'wat-lang-file-icon';
    case FileType.TOML:
      return 'toml-ext-file-icon';
    default:
      return 'txt-ext-file-icon';
  }
}

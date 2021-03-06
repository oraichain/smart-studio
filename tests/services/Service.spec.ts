/* Any copyright is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/ */

/* tslint:disable:no-empty */

declare var monaco: { MarkerSeverity };

enum Language {
  C = "c",
  Cpp = "cpp",
  Wat = "wat",
  Wasm = "wasm",
  Rust = "rust",
  Cretonne = "cton",
  x86 = "x86",
  Json = "json",
  JavaScript = "javascript",
  TypeScript = "typescript",
  Toml = "toml",
  Text = "text"
}

const createCompilerService = jest.fn();
const compile = jest.fn(() => ({ success: true, items: [] }));
const compilerService = { compile };
jest.mock("../../src/compilerServices", () => ({
  createCompilerService: createCompilerService.mockImplementation(() => compilerService),
  Language
}));



function spyOnWorker(fnName: string, mockImplementation?: () => any) {
  // @ts-ignore
  const spy = jest.spyOn((Service.worker as any), fnName);
  if (mockImplementation) {
    spy.mockImplementation(mockImplementation);
  } else {
    spy.mockImplementation(() => {});
  }
  // @ts-ignore
  (Service.worker as any).postMessage = jest.fn();
  return spy;
}

function mockFetch(returnValue) {
  (global as any).fetch = jest.fn().mockImplementation(() => Promise.resolve(returnValue));
  (global as any).Headers = jest.fn().mockImplementation((arg) => arg);
  return {
    restore: () => {
      (global as any).fetch = undefined;
      (global as any).Headers = undefined;
    }
  };
}

function trimAll(input: string) {
  return input.replace(/[\n\r]+/g, "").replace(/\s{2,}/g, "").replace(/^\s+|\s+$/, "");
}

import waitUntil from "wait-until-promise";
import { Service } from "../../src/service";
import { File, FileType, Directory, Project } from "../../src/models";
import * as taskRunner from "../../src/utils/taskRunner";
import * as rewriteSources from "../../src/utils/rewriteSources";

describe("Tests for Service", () => {
  describe("Service.getMarkers", () => {
    it("should parse the response and return an array of markers", () => {
      const response = "/tmp/build_3wzdqtu18lx.$/file.c:5:10:{5:10-5:13}: incompatible pointer to integer conversion";
      const [marker] = Service.getMarkers(response);
      expect(marker).toEqual({
        severity: monaco.MarkerSeverity.Info,
        message: "incompatible pointer to integer conversion",
        startLineNumber: 5,
        startColumn: 10,
        endLineNumber: 5,
        endColumn: 13
      });
    });
    it("should parse the response and return an array of markers (Warning)", () => {
      const response = "/tmp/build_3wzdqtu18lx.$/file.c:5:10:{5:10-5:13}: warning: incompatible pointer to integer conversion";
      const [marker] = Service.getMarkers(response);
      expect(marker).toEqual({
        severity: monaco.MarkerSeverity.Warning,
        message: "warning: incompatible pointer to integer conversion",
        startLineNumber: 5,
        startColumn: 10,
        endLineNumber: 5,
        endColumn: 13
      });
    });
    it("should parse the response and return an array of markers (Error)", () => {
      const response = `
        /tmp/build_p6pwn747hzg.$/file.c:5:7: error: incompatible pointer to integer
      `;
      const [marker] = Service.getMarkers(response);
      expect(marker).toEqual({
        severity: monaco.MarkerSeverity.Error,
        message: "error: incompatible pointer to integer",
        startLineNumber: 5,
        startColumn: 7,
        endLineNumber: 5,
        endColumn: 7
      });
    });
    it("should parse the response and return an array of markers (Range)", () => {
      const response = `
        /tmp/build_p6pwn747hzg.$/file.c:5:7:{5:11-5:15}: warning: incompatible pointer to integer
        /tmp/build_p6pwn747hzg.$/file.c:6:7:{6:11-6:15}: error: incompatible pointer to integer
      `;
      const [markerA, markerB] = Service.getMarkers(response);
      expect(markerA).toEqual({
        severity: monaco.MarkerSeverity.Warning,
        message: "warning: incompatible pointer to integer",
        startLineNumber: 5,
        startColumn: 11,
        endLineNumber: 5,
        endColumn: 15
      });
      expect(markerB).toEqual({
        severity: monaco.MarkerSeverity.Error,
        message: "error: incompatible pointer to integer",
        startLineNumber: 6,
        startColumn: 11,
        endLineNumber: 6,
        endColumn: 15
      });
    });
    it("should return an empty array if the response starts with (module", () => {
      const response = `(module
      /tmp/build_p6pwn747hzg.$/file.c:5:7:{5:11-5:15}: warning: incompatible pointer to integer
      /tmp/build_p6pwn747hzg.$/file.c:6:7:{6:11-6:15}: error: incompatible pointer to integer)
    `;
      expect(Service.getMarkers(response)).toEqual([]);
    });
  });
  describe("Service.compileFiles", () => {
   
    it("should create a compiler service", async () => {
      createCompilerService.mockClear();
      const files = [new File("file", FileType.C)];
      await Service.compileFiles(files, Language.C, Language.Wasm);
      expect(createCompilerService).toHaveBeenCalledWith("c", "wasm");
    });
    it("should compile the provided files", async () => {
      compile.mockImplementation((arg) => ({ success: true, items: arg.files }));
      const options = "-O3 -std=c++98";
      const fileA = new File("fileA.c", FileType.C);
      const fileB = new File("fileB.c", FileType.C);
      fileA.setData("fileA");
      fileB.setData("fileB");
      const result = await Service.compileFiles([fileA, fileB], Language.C, Language.Wasm, options);
      expect(compile).toHaveBeenCalledWith({
        files: {
          "fileA.c": { content: "fileA" },
          "fileB.c": { content: "fileB" },
        },
        options: "-O3 -std=c++98"
      });
      expect(result).toEqual({ "fileA.c": "fileA", "fileB.c": "fileB" });
    });
    it("should throw an error if the compilation was not successful", async () => {
      compile.mockImplementation(() => ({ success: false, items: [] }));
      const files = [new File("file", FileType.C)];
      await expect(Service.compileFiles(files, Language.C, Language.Wasm)).rejects.toThrowError();
    });
  });
  describe("Service.compileFile", () => {
    it("should compile the file with bindings", async () => {
      const compileFileWithBindings = jest.spyOn(Service, "compileFileWithBindings");
      compileFileWithBindings.mockImplementation(() => ({ wasm: "wasm" }));
      const file = new File("file.c", FileType.C);
      const result = await Service.compileFile(file, Language.C, Language.Wasm);
      expect(compileFileWithBindings).toHaveBeenCalledWith(file, Language.C, Language.Wasm, "");
      expect(result).toEqual("wasm");
      compileFileWithBindings.mockRestore();
    });
  });
  describe("Service.compileFileWithBindings", () => {
    it("should compile the file with bindings", async () => {
      const compileFiles = jest.spyOn(Service, "compileFiles");
      compileFiles.mockImplementation(() => ({ "a.wasm": { content: "wasm" }}));
      const file = new File("file.c", FileType.C);
      const result = await Service.compileFileWithBindings(file, Language.C, Language.Wasm, "options");
      expect(compileFiles).toHaveBeenCalledWith([file], Language.C, Language.Wasm, "options");
      expect(result).toEqual({ wasm: { content: "wasm" }});
      expect(result).not.toHaveProperty("wasmBindgenJs");
      compileFiles.mockRestore();
    });
    it("should compile the file with bindings (wasm_bindgen.js)", async () => {
      const compileFiles = jest.spyOn(Service, "compileFiles");
      compileFiles.mockImplementation(() => ({ "a.wasm": { content: "wasm" }, "wasm_bindgen.js": { content: "bindgen" }}));
      const file = new File("file.c", FileType.C);
      const result = await Service.compileFileWithBindings(file, Language.C, Language.Wasm, "options");
      expect(compileFiles).toHaveBeenCalledWith([file], Language.C, Language.Wasm, "options");
      expect(result).toEqual({
        wasm: { content: "wasm" },
        wasmBindgenJs: { content: "bindgen" }
      });
      compileFiles.mockRestore();
    });
    it("should throw an error for non wasm target", async () => {
      const file = new File("file.c", FileType.C);
      const compilePromise = Service.compileFileWithBindings(file, Language.C, Language.x86, "options");
      const expectedErrorMessage = `Only wasm target is supported, but "x86" was found`;
      await expect(compilePromise).rejects.toThrowError(expectedErrorMessage);
    });
  });
  describe("Service.loadJSON", () => {
    it("should load a fiddle from the provided uri", async () => {
      const json = jest.fn(() => Promise.resolve("response"));
      const { restore } = mockFetch({ json });
      const result = await Service.loadJSON("uri");
      expect(window.fetch).toHaveBeenCalledWith("https://webassembly-studio-fiddles.herokuapp.com/fiddle/uri", {
        headers: { "Content-type": "application/json; charset=utf-8" }
      });
      expect(result).toEqual("response");
      restore();
    });
  });
  describe("Service.saveJSON", () => {
    it("should save the provided JSON as a fiddle and return the uri", async () => {
      const id = "https://webassembly-studio-fiddles.herokuapp.com/fiddle/id";
      const json = jest.fn(() => Promise.resolve({ id }));
      const { restore } = mockFetch({ json });
      const result = await Service.saveJSON({ a: 1, b: 2 } as any, null);
      expect(window.fetch).toHaveBeenCalledWith("https://webassembly-studio-fiddles.herokuapp.com/set-fiddle", {
        method: "POST",
        headers: new Headers({ "Content-type": "application/json; charset=utf-8" }),
        body: JSON.stringify({ a: 1, b: 2 })
      });
      expect(result).toEqual("id");
      restore();
    });
    it("should throw an error on updates", async () => {
      const id = "https://webassembly-studio-fiddles.herokuapp.com/fiddle/id";
      const json = jest.fn(() => Promise.resolve({ id }));
      const { restore } = mockFetch({ json });
      const savePromise = Service.saveJSON({ a: 1, b: 2 } as any, "uri");
      await expect(savePromise).rejects.toThrowError();
      restore();
    });
  });
  describe("Service.parseFiddleURI", () => {
    it("should return the fiddle uri", () => {
      window.history.pushState({}, "", "https://webassembly.studio/?f=htctf3gp1ws");
      expect(Service.parseFiddleURI()).toEqual("f=htctf3gp1ws");
    });
    it("should handle cases where the uri contains / ", () => {
      window.history.pushState({}, "", "https://webassembly.studio/?f=htctf3gp1ws/");
      expect(Service.parseFiddleURI()).toEqual("f=htctf3gp1ws");
    });
    it("should handle cases where no fiddle uri is present", () => {
      window.history.pushState({}, "", "https://webassembly.studio/");
      expect(Service.parseFiddleURI()).toEqual("");
    });
  });
  
  describe("Service.connectWallet", () => {
    it("should export the provided file to a wallet", async () => {
      const connectWallet = jest.spyOn(Service, "connectWallet");
      connectWallet.mockImplementation(() => {});
      const file = new File("file.js", FileType.JavaScript);
      file.setData("file-data");
      await Service.exportToWallet(file);
      expect(connectWallet).toHaveBeenCalledWith({
        description: "source: https://webassembly.studio",
        public: true,
        files: { "file.js": { content: "file-data" }}
      });
      connectWallet.mockRestore();
    });
    it("should export the provided directory to a wallet", async () => {
      const connectWallet = jest.spyOn(Service, "connectWallet");
      connectWallet.mockImplementation(() => {});
      const directory = new Directory("src");
      const fileA = directory.newFile("fileA.js", FileType.JavaScript);
      const fileB = directory.newFile("fileB.js", FileType.JavaScript);
      fileA.setData("file-data");
      fileB.isTransient = true;
      await Service.exportToWallet(directory);
      expect(connectWallet).toHaveBeenCalledWith({
        description: "source: https://webassembly.studio",
        public: true,
        files: { "fileA.js": { content: "file-data" }}
      });
      connectWallet.mockRestore();
    });
    it("should be possible to provide a fiddle url as an optional parameter", async () => {
      const connectWallet = jest.spyOn(Service, "connectWallet");
      connectWallet.mockImplementation(() => {});
      const file = new File("file.js", FileType.JavaScript);
      file.setData("file-data");
      await Service.exportToWallet(file, "fiddle-uri");
      expect(connectWallet).toHaveBeenCalledWith({
        description: "source: https://webassembly.studio/?f=fiddle-uri",
        public: true,
        files: { "file.js": { content: "file-data" }}
      });
      connectWallet.mockRestore();
    });
  });

  describe("Service.saveProject", () => {
    it("should save the project to a fiddle", async () => {
      const saveJSON = jest.spyOn(Service, "saveJSON");
      saveJSON.mockImplementation(() => {});
      const project = new Project();
      const directory = project.newDirectory("src");
      const fileA = directory.newFile("fileA.c", FileType.C);
      const fileB = directory.newFile("fileB.wasm", FileType.Wasm);
      const fileC = project.newFile("fileC.js", FileType.JavaScript);
      fileA.setData("fileA-data");
      fileB.setData(new ArrayBuffer(8));
      fileC.isTransient = true;
      await Service.saveProject(project, []);
      expect(saveJSON).toHaveBeenCalledWith({
        files: [
          { name: "src/fileA.c", type: "text", data: "fileA-data" },
          { name: "src/fileB.wasm", type: "binary", data: "AAAAAAAAAAA=" }
        ]
      }, undefined);
      saveJSON.mockRestore();
    });
  });
  describe("Service.loadFilesIntoProject", () => {
    it("should load the provided files into the project", async () => {
      const project = new Project();
      const fileA = { name: "fileA.c", data: "fileA-data", type: "text" };
      const fileB = { name: "fileB.wasm", data: "AAAAAAAAAAA=", type: "binary" };
      await Service.loadFilesIntoProject([fileA, fileB] as any, project);
      const createdFileA = project.getFile("fileA.c");
      const createdFileB = project.getFile("fileB.wasm");
      expect(createdFileA.getData()).toEqual("fileA-data");
      expect(createdFileA.type).toEqual(FileType.C);
      expect(createdFileA.isTransient).toEqual(false);
      expect(createdFileB.getData()).toEqual(new ArrayBuffer(8));
      expect(createdFileB.type).toEqual(FileType.Wasm);
      expect(createdFileB.isTransient).toEqual(false);
    });
    it("should fetch files that does not yet contain any data (text)", async () => {
      const text = jest.fn(() => Promise.resolve("fetched-file-data"));
      const { restore } = mockFetch({ text });
      const project = new Project();
      const fileA = { name: "fileA.c", type: "text" };
      const baseURL = new URL("https://webassembly.studio/");
      await Service.loadFilesIntoProject([fileA] as any, project, baseURL);
      const createdFileA = project.getFile("fileA.c");
      expect(createdFileA.getData()).toEqual("fetched-file-data");
      expect(createdFileA.type).toEqual(FileType.C);
      expect(createdFileA.isTransient).toEqual(false);
      expect(window.fetch).toHaveBeenCalledWith("https://webassembly.studio/fileA.c");
      restore();
    });
    it("should fetch files that does not yet contain any data (binary)", async () => {
      const arrayBuffer = jest.fn(() => Promise.resolve("fetched-array-buffer"));
      const { restore } = mockFetch({ arrayBuffer });
      const project = new Project();
      const fileA = { name: "fileA.wasm", type: "binary" };
      const baseURL = new URL("https://webassembly.studio/");
      await Service.loadFilesIntoProject([fileA] as any, project, baseURL);
      const createdFileA = project.getFile("fileA.wasm");
      expect(createdFileA.getData()).toEqual("fetched-array-buffer");
      expect(createdFileA.type).toEqual(FileType.Wasm);
      expect(createdFileA.isTransient).toEqual(false);
      expect(window.fetch).toHaveBeenCalledWith("https://webassembly.studio/fileA.wasm");
      restore();
    });
  });
  describe("Service.lazyLoad", () => {
    it("should lazy load a script from the specified uri", async () => {
      const scriptElement = {};
      const createElement = jest.spyOn(window.document, "createElement");
      createElement.mockImplementation(() => scriptElement);
      const appendChild = jest.spyOn(window.document.body, "appendChild");
      appendChild.mockImplementation(() => {});
      const status = { push: jest.fn(), pop: jest.fn() } as any;
      Service.lazyLoad("uri", status);
      await waitUntil(() => (scriptElement as any).onload); // Wait until the onload fn is defined
      (scriptElement as any).onload();
      expect(createElement).toHaveBeenLastCalledWith("script");
      expect(appendChild).toHaveBeenCalledWith(scriptElement);
      expect(status.push).toHaveBeenCalledWith("Loading uri");
      expect(status.pop).toHaveBeenCalled();
      expect((scriptElement as any).async).toEqual(true);
      expect((scriptElement as any).src).toEqual("uri");
    });
  });
  
  describe("Service.download", () => {
    it("should create a hidden link and download the provided file", () => {
      const link = { click: jest.fn(), style: {} } as any;
      const createElement = jest.spyOn(document, "createElement");
      const appendChild = jest.spyOn(document.body, "appendChild");
      createElement.mockImplementation(() => link);
      appendChild.mockImplementation(() => {});
      const createObjectURL = jest.fn();
      window.URL.createObjectURL = createObjectURL;
      const file = new File("file.js", FileType.JavaScript);
      file.setData("file-data");
      Service.download(file);
      expect(createElement).toHaveBeenCalledWith("a");
      expect(appendChild).toHaveBeenCalledWith(link);
      expect(link.style.display).toEqual("none");
      expect(link.download).toEqual("file.js");
      expect(link.click).toHaveBeenCalled();
      expect(createObjectURL.mock.calls[0][0].size).toEqual(9);
      expect(createObjectURL.mock.calls[0][0].type).toEqual("application/octet-stream");
      createElement.mockRestore();
      appendChild.mockRestore();
    });
    it("should only create a link if one does not already exist", () => {
      const createElement = jest.spyOn(document, "createElement");
      const createObjectURL = jest.fn();
      window.URL.createObjectURL = createObjectURL;
      const file = new File("file.js", FileType.JavaScript);
      file.setData("file-data");
      Service.download(file);
      expect(createElement).not.toHaveBeenCalled();
      createElement.mockRestore();
    });
  });
  describe("Service.clangFormat", () => {
    beforeAll(() => {
      (global as any).Module = jest.fn(() => ({ ccall: jest.fn(() => "formated") }));
    });
   
    it("should only load clang-format.js if it hasn't already been loaded", async () => {
      const lazyLoad = jest.spyOn(Service, "lazyLoad");
      lazyLoad.mockImplementation(() => {});
      const arrayBuffer = jest.fn(() => Promise.resolve("buffer"));
      const { restore } = mockFetch({ arrayBuffer });
      const file = new File("file.c", FileType.C);
      const setValue = jest.spyOn(file.buffer, "setValue");
      const status = { push: jest.fn(), pop: jest.fn() } as any;
      await Service.clangFormat(file, status);
      expect(Service.clangFormatModule.ccall).toHaveBeenCalledWith("formatCode", "string", ["string"], [undefined]);
      expect(lazyLoad).not.toHaveBeenCalled();
      expect(setValue).toHaveBeenCalledWith("formated");
      lazyLoad.mockRestore();
      restore();
    });
  });


  
  describe("Service.openBinaryExplorer", () => {
    it("should open the binary explorer", () => {
      const open = jest.spyOn((window as any), "open");
      const removeEventListener = jest.spyOn(window, "removeEventListener");
      open.mockImplementation(() => {});
      const file = new File("file.js", FileType.JavaScript);
      Service.openBinaryExplorer(file);
      expect(open).toHaveBeenCalledWith(
        "//wasdk.github.io/wasmcodeexplorer/?api=postmessage",
        "",
        "toolbar=no,ocation=no,directories=no,status=no,menubar=no,location=no,scrollbars=yes,resizable=yes,width=1024,height=568"
      );
      expect(removeEventListener).not.toHaveBeenCalled();
      removeEventListener.mockRestore();
      open.mockRestore();
    });
    it("should handle message events", () => {
      const open = jest.spyOn((window as any), "open");
      open.mockImplementation(() => {});
      const removeEventListener = jest.spyOn(window, "removeEventListener");
      const event = new Event("message") as any;
      (event as any).data = { type: "wasmexplorer-ready" };
      (event as any).source = { postMessage: jest.fn() };
      const file = new File("file.js", FileType.JavaScript);
      file.setData("file-data");
      Service.openBinaryExplorer(file);
      window.dispatchEvent(event);
      expect(removeEventListener.mock.calls[0][0]).toEqual("message");
      expect(removeEventListener.mock.calls[1][0]).toEqual("message");
      expect(removeEventListener).toHaveBeenCalledTimes(2);
      expect(event.source.postMessage).toHaveBeenCalledWith({
        type: "wasmexplorer-load",
        data: new Uint8Array(0)
      }, "*", [new ArrayBuffer(8)]);
      removeEventListener.mockRestore();
      open.mockRestore();
    });
  });
  describe("Service.import", () => {
    it("should create a script tag and import the script from the provided path", async () => {
      const scriptElement = document.createElement("script");
      const remove = jest.spyOn(scriptElement, "remove");
      const project = new Project();
      const context = {} as any;
      const global = {
        document: {
          createElement: jest.fn(() => scriptElement),
          head: { appendChild: jest.fn(() => {}) }
        },
        Blob: Blob,
        URL: { createObjectURL: (arg) => arg }
      };
      const random = jest.spyOn(Math, "random");
      const getCurrentRunnerInfo = jest.spyOn(taskRunner, "getCurrentRunnerInfo");
      const RewriteSourcesContext = jest.spyOn(rewriteSources, "RewriteSourcesContext");
      const processJSFile = jest.spyOn(rewriteSources, "processJSFile");
      getCurrentRunnerInfo.mockImplementation(() => ({ project, global }));
      processJSFile.mockImplementation(() => "url");
      RewriteSourcesContext.mockImplementation(() => context);
      random.mockImplementation(() => 0.87);
      Service.import("script-path");
      const id = scriptElement.textContent.substr(26, 20);
      const makeReady = global[id];
      const fileBlob = context.createFile("test", "testtype");
      expect(context.logLn).toBe(console.log);
      expect(fileBlob.size).toEqual(4);
      expect(fileBlob.type).toEqual("testtype");
      expect(scriptElement.type).toEqual("module");
      expect(scriptElement.getAttribute("async")).toEqual("async");
      expect(scriptElement.textContent.substr(0, 25)).toEqual("import * as i from 'url';");
      expect(global.document.createElement).toHaveBeenCalledWith("script");
      expect(global.document.head.appendChild).toHaveBeenCalledWith(scriptElement);
      expect(processJSFile).toHaveBeenCalledWith(context, "script-path");
      expect(RewriteSourcesContext).toHaveBeenCalledWith(project);
      await makeReady(); // Fake that is ready
      await waitUntil(() => !global[id]); // Wait until the makeReady fn has finished
      expect(global[id]).toBeUndefined();
      expect(remove).toHaveBeenCalled();
      random.mockRestore();
      getCurrentRunnerInfo.mockRestore();
      RewriteSourcesContext.mockRestore();
      processJSFile.mockRestore();
    });
  });
  describe("Service.compileMarkdownToHtml", () => {
    it("should load lib/showdown.min.js and convert the provided markdown to html", async () => {
      const lazyLoad = jest.spyOn(Service, "lazyLoad");
      lazyLoad.mockImplementation(() => {
        (global as any).showdown = {
          setFlavor: jest.fn(),
          Converter: jest.fn(() => ({
            makeHtml: jest.fn((arg) => arg + "-has-been-compiled")
          }))
        };
      });
      const result = await Service.compileMarkdownToHtml("markdown");
      expect(lazyLoad).toHaveBeenCalledWith("lib/showdown.min.js");
      expect(result).toEqual("markdown-has-been-compiled");
      expect((global as any).showdown.Converter).toHaveBeenCalledWith({ tables: true, ghCodeBlocks: true });
      expect((global as any).showdown.setFlavor).toHaveBeenCalledWith("github");
      lazyLoad.mockRestore();
    });
    it("should only load lib/showdown.min.js if it hasn't already been loaded", async () => {
      const lazyLoad = jest.spyOn(Service, "lazyLoad");
      lazyLoad.mockImplementation(() => {
        (global as any).showdown = {
          setFlavor: jest.fn(),
          Converter: jest.fn(() => ({
            makeHtml: jest.fn((arg) => arg + "-has-been-compiled")
          }))
        };
      });
      await Service.compileMarkdownToHtml("markdown");
      expect(lazyLoad).not.toHaveBeenCalled();
      lazyLoad.mockRestore();
    });
  });

});
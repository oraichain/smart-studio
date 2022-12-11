import * as monaco from 'monaco-editor';
import { File, FileType, languageForFileType } from '../models';
import { WorldState } from '../../crates/ra-wasm/pkg';
import { createRA } from './creat-ra';
import rust_std from '../rust/std.rs';
import rust_core from '../rust/core.rs';
import rust_alloc from '../rust/alloc.rs';
import rust_cosmwasm_derive from '../rust/cosmwasm-derive.rs';
import rust_cosmwasm_schema_derive from '../rust/cosmwasm-schema-derive.rs';
import rust_cosmwasm_schema from '../rust/cosmwasm-schema.rs';
import rust_cosmwasm_std from '../rust/cosmwasm-std.rs';
import rust_cosmwasm_crypto from '../rust/cosmwasm-crypto.rs';
import rust_cosmwasm_storage from '../rust/cosmwasm-storage.rs';

window.MonacoEnvironment = {
  getWorkerUrl: () => './editor.worker.bundle.js'
};

export class LanguageUpdater {
  private states: Map<monaco.Uri, WorldState> = new Map();
  private languageId: string;
  constructor(fileType: FileType) {
    this.languageId = languageForFileType(fileType);
  }

  private async addModel(model: monaco.editor.ITextModel) {
    // const { WorldState: WorldStateClass } = await import('../../lib/analyzer/pkg');
    let uri = model.uri;
    if (this.states.has(uri)) return;

    const state = (await createRA()) as WorldState;

    await state.init(
      model.getValue(),
      rust_std,
      rust_core,
      rust_alloc,
      rust_cosmwasm_derive,
      rust_cosmwasm_schema_derive,
      rust_cosmwasm_schema,
      rust_cosmwasm_std,
      rust_cosmwasm_crypto,
      rust_cosmwasm_storage
    );

    const update = async () => {
      const res = await state.update(model.getValue());
      monaco.editor.setModelMarkers(model, this.languageId, res.diagnostics);
    };
    await update();
    model.onDidChangeContent(update);

    this.states.set(uri, state);
  }

  addFile(file: File) {
    const { buffer, type } = file;
    const languageId = languageForFileType(type);
    if (languageId === this.languageId) {
      return this.addModel(buffer);
    }
  }

  async provideHover(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    const hoverData = await state.hover(pos.lineNumber, pos.column);
    return hoverData;
  }

  async provideCodeLenses(model: monaco.editor.ITextModel) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const codeLenses = await state.code_lenses();
    const lenses = codeLenses.map(({ range, command }: any) => {
      const position = {
        column: range.startColumn,
        lineNumber: range.startLineNumber
      };

      const references = command.positions.map((pos: any) => ({
        range: pos,
        uri: model.uri
      }));
      return {
        range,
        command: {
          id: command.id,
          title: command.title,
          arguments: [model.uri, position, references]
        }
      };
    });

    return { lenses, dispose() {} };
  }

  async provideReferences(model: monaco.editor.ITextModel, pos: monaco.Position, { includeDeclaration }: any) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const references = await state.references(pos.lineNumber, pos.column, includeDeclaration);
    if (references) {
      return references.map(({ range }: any) => ({ uri: model.uri, range }));
    }
  }

  async provideDocumentHighlights(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const references = state.references(pos.lineNumber, pos.column, true);
    return references;
  }

  async provideRenameEdits(model: monaco.editor.ITextModel, pos: monaco.Position, newName: string) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const edits = await state.rename(pos.lineNumber, pos.column, newName);
    if (edits) {
      return {
        edits: edits.map((edit: any) => ({
          resource: model.uri,
          edit
        }))
      };
    }
  }

  async resolveRenameLocation(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    return await state.prepare_rename(pos.lineNumber, pos.column);
  }

  async provideCompletionItems(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const suggestions = await state.completions(pos.lineNumber, pos.column);
    if (suggestions) {
      return { suggestions };
    }
  }

  async provideInlayHints(model: monaco.editor.ITextModel, range: monaco.Range, token: monaco.CancellationToken) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const inlayHints = await state.inlay_hints();
    return inlayHints;
  }

  async provideSignatureHelp(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const value = await state.signature_help(pos.lineNumber, pos.column);
    if (value) {
      return {
        value,
        dispose() {}
      };
    }
  }

  async provideDefinition(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const list = await state.definition(pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: any) => ({ ...def, uri: model.uri }));
    }
  }

  async provideTypeDefinition(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const list = await state.type_definition(pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: any) => ({ ...def, uri: model.uri }));
    }
  }

  async provideImplementation(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const list = await state.goto_implementation(pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: any) => ({ ...def, uri: model.uri }));
    }
  }

  async provideDocumentSymbols(model: monaco.editor.ITextModel) {
    const state = this.states.get(model.uri);
    if (!state) return;
    return await state.document_symbols();
  }

  async provideOnTypeFormattingEdits(model: monaco.editor.ITextModel, pos: monaco.Position, ch: string) {
    const state = this.states.get(model.uri);
    if (!state) return;
    return await state.type_formatting(pos.lineNumber, pos.column, ch);
  }

  async provideFoldingRanges(model: monaco.editor.ITextModel) {
    const state = this.states.get(model.uri);
    if (!state) return;
    return await state.folding_ranges();
  }
}

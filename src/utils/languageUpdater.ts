import { extensionForFileType, File, FileType, languageForFileType, nameForFileType } from '../models';
import { WorldState } from '../../lib/analyzer/pkg';

export class LanguageUpdater {
  private states: Map<monaco.Uri, WorldState> = new Map();
  private languageId: string;
  constructor(lib: string, fileType: FileType) {
    this.languageId = nameForFileType(fileType);
    const uri = monaco.Uri.file('/libs.' + extensionForFileType(fileType));
    const textModel = monaco.editor.createModel(lib, this.languageId, uri);
    this.addModel(textModel);
  }

  private async addModel(model: monaco.editor.ITextModel) {
    const { WorldState: WorldStateClass } = await import('../../lib/analyzer/pkg');
    let uri = model.uri;
    if (this.states.has(uri)) return;
    const state = new WorldStateClass();
    const update = () => {
      const res = state.update(model.getValue());
      monaco.editor.setModelMarkers(model, this.languageId, res.diagnostics);
    };
    update();
    model.onDidChangeContent(update);
    this.states.set(uri, state);
  }

  private provideByFunction(
    model: monaco.editor.ITextModel,
    pos: monaco.Position,
    check: (data: any) => boolean,
    process: (otherModel: monaco.editor.ITextModel, otherPos: monaco.Position) => any
  ): any {
    let data = process(model, pos);
    // process other
    if (!check(data)) {
      // try other states
      const word = model.getWordAtPosition(pos);

      for (let uri of this.states.keys()) {
        if (uri.path !== model.uri.path) {
          const otherModel = monaco.editor.getModel(uri);
          // need to sync, model may be deleted
          if (!otherModel) {
            this.states.delete(uri);
            continue;
          }
          const index = otherModel.getValue().lastIndexOf(word.word);
          if (index === -1) continue;
          const otherPos = otherModel.getPositionAt(index + 1);
          data = process(otherModel, otherPos);
          if (data) break;
        }
      }
    }
    return data;
  }

  addFile(file: File) {
    const { buffer, type } = file;
    const languageId = languageForFileType(type);
    if (languageId === this.languageId) {
      return this.addModel(buffer);
    }
  }

  provideHover(model: monaco.editor.ITextModel, pos: monaco.Position) {
    // process hover
    return this.provideByFunction(
      model,
      pos,
      (data: any) => data && !data.contents[0].value.match(/rust\n*{unknown}/),
      (otherModel: monaco.editor.ITextModel, otherPos: monaco.Position) => {
        const otherState = this.states.get(otherModel.uri);
        if (!otherState) return;
        return otherState.hover(otherPos.lineNumber, otherPos.column);
      }
    );
  }

  provideCodeLenses(model: monaco.editor.ITextModel) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const code_lenses = state.code_lenses();
    const lenses = code_lenses.map(({ range, command }: any) => {
      const position = {
        column: range.startColumn,
        lineNumber: range.startLineNumber
      };

      const references: monaco.languages.Location = command.positions.map((pos: any) => ({
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

  provideReferences(model: monaco.editor.ITextModel, pos: monaco.Position, { includeDeclaration }: monaco.languages.ReferenceContext) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const references = state.references(pos.lineNumber, pos.column, includeDeclaration);
    if (references) {
      return references.map(({ range }: monaco.languages.Location) => ({ uri: model.uri, range }));
    }
  }

  provideDocumentHighlights(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    return state.references(pos.lineNumber, pos.column, true);
  }

  provideRenameEdits(model: monaco.editor.ITextModel, pos: monaco.Position, newName: string) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const edits = state.rename(pos.lineNumber, pos.column, newName);
    if (edits) {
      return {
        edits: [
          {
            resource: model.uri,
            edits
          }
        ]
      };
    }
  }

  resolveRenameLocation(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    return state.prepare_rename(pos.lineNumber, pos.column);
  }

  provideCompletionItems(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const suggestions = state.completions(pos.lineNumber, pos.column);
    if (suggestions) {
      return { suggestions };
    }
  }

  provideSignatureHelp(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const value = state.signature_help(pos.lineNumber, pos.column);
    if (value) {
      return {
        value,
        dispose() {}
      };
    }
  }

  provideDefinition(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const list = state.definition(pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: monaco.languages.Definition) => ({ ...def, uri: model.uri }));
    }
  }

  provideTypeDefinition(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const list = state.type_definition(pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: monaco.languages.Definition) => ({ ...def, uri: model.uri }));
    }
  }

  provideImplementation(model: monaco.editor.ITextModel, pos: monaco.Position) {
    const state = this.states.get(model.uri);
    if (!state) return;
    const list = state.goto_implementation(pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: monaco.languages.Definition) => ({ ...def, uri: model.uri }));
    }
  }

  provideDocumentSymbols(model: monaco.editor.ITextModel) {
    const state = this.states.get(model.uri);
    if (!state) return;
    return state.document_symbols();
  }

  provideOnTypeFormattingEdits(model: monaco.editor.ITextModel, pos: monaco.Position, ch: string) {
    const state = this.states.get(model.uri);
    if (!state) return;
    return state.type_formatting(pos.lineNumber, pos.column, ch);
  }

  provideFoldingRanges(model: monaco.editor.ITextModel) {
    const state = this.states.get(model.uri);
    if (!state) return;
    return state.folding_ranges();
  }
}

import * as monaco from 'monaco-editor';
import { File, FileType, languageForFileType } from '../models';
import { WorldState } from '../../crates/ra-wasm/pkg';
import { createRA } from './create-ra';
import { Language } from '../compilerServices';

window.MonacoEnvironment = {
  getWorkerUrl: (moduleId, label) => {
    if (label === 'json') {
      return './json.worker.bundle.js';
    }
    return './editor.worker.bundle.js';
  }
};

export type Token = {
  range: monaco.IRange;
  tag: string;
};

class TokenState {
  line: number;
  equals: (other?: monaco.languages.IState) => boolean;
  constructor(line = 0) {
    this.line = line;
    this.equals = () => true;
  }

  clone() {
    const res = new TokenState(this.line);
    res.line += 1;
    return res;
  }
}

export class LanguageUpdater {
  static contractFiles: string[] = ['lib.rs'];
  private state: WorldState;
  private fileIdMap: Map<monaco.Uri, number> = new Map();
  private languageId: string;
  constructor(fileType: FileType) {
    this.languageId = languageForFileType(fileType);
  }

  async initialize() {
    if (this.state) return;
    this.state = (await createRA()) as WorldState;
    if (process.env.NODE_ENV === 'production') {
      const data = await fetch('/change.json');
      const textData = await data.text();
      const encoder = new TextEncoder();
      this.state.load(encoder.encode(textData));
    } else {
      // fallback loading from source code
      const rustFiles = await Promise.all([
        import('../rust/libstd.rs'),
        import('../rust/libcore.rs'),
        import('../rust/liballoc.rs'),
        import('../rust/cosmwasm-derive.rs'),
        import('../rust/cosmwasm-schema-derive.rs'),
        import('../rust/cosmwasm-schema.rs'),
        import('../rust/cosmwasm-std.rs'),
        import('../rust/cosmwasm-crypto.rs'),
        import('../rust/cosmwasm-storage.rs'),
        import('../rust/thiserror-1.0.23.rs'),
        import('../rust/thiserror-impl-1.0.23.rs'),
        import('../rust/proc-macro2-1.0.6.rs')
      ]);

      // @ts-ignore
      this.state.init(...rustFiles.map((m) => m.default));
    }
  }

  private setTokens(allTokens: Token[]) {
    monaco.languages.setTokensProvider(Language.RustAnalyzer, {
      getInitialState: () => new TokenState(),
      tokenize(_, st: TokenState) {
        const filteredTokens = allTokens.filter((token) => token.range.startLineNumber === st.line);

        const tokens = filteredTokens.map((token) => ({
          startIndex: token.range.startColumn - 1,
          scopes: token.tag
        }));
        tokens.sort((a, b) => a.startIndex - b.startIndex);

        return {
          tokens,
          endState: new TokenState(st.line + 1)
        };
      }
    });
  }

  private async addModel(model: monaco.editor.ITextModel) {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const update = async () => {
      const res = await this.state.update(fileInd, model.getValue());
      monaco.editor.setModelMarkers(model, Language.RustAnalyzer, res.diagnostics);
      // update token highlights
      this.setTokens(res.highlights);
    };

    // const start = Date.now();
    await update();
    // console.log('Took', Date.now() - start, 'ms');
    model.onDidChangeContent(update);
    monaco.editor.setModelLanguage(model, Language.RustAnalyzer);
  }

  addFile(file: File) {
    const { buffer, type, name } = file;
    const languageId = languageForFileType(type);
    if (languageId === this.languageId) {
      // update all source code for rust
      const fileInd = LanguageUpdater.contractFiles.indexOf(name);
      this.fileIdMap.set(buffer.uri, fileInd);
      return this.addModel(buffer);
    }
  }

  async provideHover(model: monaco.editor.ITextModel, pos: monaco.Position):Promise<monaco.languages.Hover> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const content:monaco.languages.Hover = await this.state.hover(fileInd, pos.lineNumber, pos.column);
    // this fixes rust-ra language at client
    if (content?.range) {
      return {
        contents: [
          {
            ...content.contents[0],
            value: JSON.parse(
              JSON.stringify(content.contents[0]?.value)
                .replaceAll(/```(.*?)```/g, '```rust$1```')
                .replaceAll('rustrust', 'rust')                
            ),
            supportThemeIcons: true,
          },
        ],
        range: content.range,
      } as monaco.languages.Hover;
    }
  }

  async provideCodeLenses(model: monaco.editor.ITextModel) {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const codeLenses = await this.state.code_lenses(fileInd);
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

  async provideReferences(model: monaco.editor.ITextModel, pos: monaco.Position, { includeDeclaration }: monaco.languages.ReferenceContext): Promise<monaco.languages.Location[]> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const references: monaco.languages.Location[] = await this.state.references(fileInd, pos.lineNumber, pos.column, includeDeclaration);
    if (references) {
      return references.map(({ range }) => ({ uri: model.uri, range }));
    }
  }

  async provideDocumentHighlights(model: monaco.editor.ITextModel, pos: monaco.Position): Promise<monaco.languages.DocumentHighlight[]> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const references = this.state.references(fileInd, pos.lineNumber, pos.column, true);
    return references;
  }

  async provideRenameEdits(model: monaco.editor.ITextModel, pos: monaco.Position, newName: string): Promise<monaco.languages.WorkspaceEdit> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const edits: monaco.languages.TextEdit[] = await this.state.rename(fileInd, pos.lineNumber, pos.column, newName);
    if (edits) {
      return {
        edits: edits.map((textEdit) => ({
          textEdit,
          versionId: 0,
          resource: model.uri
        }))
      };
    }
  }

  async resolveRenameLocation(model: monaco.editor.ITextModel, pos: monaco.Position): Promise<monaco.languages.RenameLocation> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    return await this.state.prepare_rename(fileInd, pos.lineNumber, pos.column);
  }

  async provideCompletionItems(model: monaco.editor.ITextModel, pos: monaco.Position): Promise<monaco.languages.CompletionList> {
    const fileInd = this.fileIdMap.get(model.uri);

    if (fileInd === -1) return;
    const suggestions = await this.state.completions(fileInd, pos.lineNumber, pos.column);

    if (suggestions) {
      return { suggestions };
    }
  }

  async provideInlayHints(model: monaco.editor.ITextModel): Promise<monaco.languages.InlayHintList> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const inlayHints = await this.state.inlay_hints(fileInd);
    if (!inlayHints) return;

    return {
      hints: inlayHints.map(({ label, hint_type, range }: any) => {
        return {
          label,
          kind: hint_type,
          paddingRight: true,
          position: {
            column: range.startColumn,
            lineNumber: range.startLineNumber
          }
        };
      }),
      dispose() {}
    };
  }

  async provideSignatureHelp(model: monaco.editor.ITextModel, pos: monaco.Position): Promise<monaco.languages.SignatureHelpResult> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const value = await this.state.signature_help(fileInd, pos.lineNumber, pos.column);
    if (value) {
      return {
        value,
        dispose() {}
      };
    }
  }

  async provideDefinition(model: monaco.editor.ITextModel, pos: monaco.Position): Promise<monaco.languages.Definition> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const list = await this.state.definition(fileInd, pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: any) => ({ ...def, uri: model.uri }));
    }
  }

  async provideTypeDefinition(model: monaco.editor.ITextModel, pos: monaco.Position): Promise<monaco.languages.Definition> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const list = await this.state.type_definition(fileInd, pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: any) => ({ ...def, uri: model.uri }));
    }
  }

  async provideImplementation(model: monaco.editor.ITextModel, pos: monaco.Position): Promise<monaco.languages.Definition> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    const list = await this.state.goto_implementation(fileInd, pos.lineNumber, pos.column);
    if (list) {
      return list.map((def: any) => ({ ...def, uri: model.uri }));
    }
  }

  async provideDocumentSymbols(model: monaco.editor.ITextModel): Promise<monaco.languages.DocumentSymbol[]> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    return await this.state.document_symbols(fileInd);
  }

  async provideOnTypeFormattingEdits(model: monaco.editor.ITextModel, pos: monaco.Position, ch: string): Promise<monaco.languages.TextEdit[]> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    return await this.state.type_formatting(fileInd, pos.lineNumber, pos.column, ch);
  }

  async provideFoldingRanges(model: monaco.editor.ITextModel): Promise<monaco.languages.FoldingRange[]> {
    const fileInd = this.fileIdMap.get(model.uri);
    if (fileInd === -1) return;
    return await this.state.folding_ranges(fileInd);
  }
}

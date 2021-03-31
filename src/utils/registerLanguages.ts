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

import * as rustConf from 'monaco-editor/esm/vs/basic-languages/rust/rust';
import { WorldState } from 'wasm-analyzer';

export interface Token {
  tag: string;
  range: monaco.Range;
}

let state: WorldState = null;
let allTokens: Token[] = [];

// default we will update tokens from all open files
export const updateModelTokens = async (model: monaco.editor.ITextModel, languageId: string) => {
  let update;

  switch (languageId) {
    case 'ra-rust':
      // need to merge
      update = () => {
        const res = state.update(model.getValue());
        monaco.editor.setModelMarkers(model, languageId, res.diagnostics);
        // TODO: completion from all files
        if (res.highlights.length) {
          allTokens = res.highlights;
        }
      };
      break;
    default:
      break;
  }
  if (update && state) {
    update();
    model.onDidChangeContent(update);
  }
};

class State implements monaco.languages.IState {
  clone() {
    return new State();
  }
  equals(other: monaco.languages.IState): boolean {
    return other === this;
  }
}

class TokenState implements monaco.languages.IState {
  public line: number;
  constructor(line = 0) {
    this.line = line;
  }

  equals(other: monaco.languages.IState): boolean {
    return true;
  }

  clone(): monaco.languages.IState {
    const res = new TokenState(this.line);
    res.line += 1;
    return res;
  }
}

const fixTag = (tag: string) => {
  switch (tag) {
    case 'builtin':
      return 'variable.predefined';
    case 'attribute':
      return 'key';
    case 'macro':
      return 'number.hex';
    case 'literal':
      return 'number';
    default:
      return tag;
  }
};

export default async function registerLanguages() {
  const { WorldState: WorldStateClass } = await import('wasm-analyzer');
  state = new WorldStateClass();

  monaco.languages.register({
    id: 'rust'
  });
  monaco.languages.setMonarchTokensProvider('rust', rustConf.language);

  // Rust analyzer implementation
  monaco.languages.register({
    id: 'ra-rust'
  });

  monaco.languages.setLanguageConfiguration('ra-rust', rustConf.conf);

  monaco.languages.onLanguage('ra-rust', async () => {
    monaco.languages.registerHoverProvider('ra-rust', {
      provideHover: (_, pos) => state.hover(pos.lineNumber, pos.column)
    });
    monaco.languages.registerCodeLensProvider('ra-rust', {
      provideCodeLenses(m) {
        const code_lenses = state.code_lenses();
        const lenses = code_lenses.map(({ range, command }: any) => {
          const position = {
            column: range.startColumn,
            lineNumber: range.startLineNumber
          };

          const references: monaco.languages.Location = command.positions.map((pos: any) => ({
            range: pos,
            uri: m.uri
          }));
          return {
            range,
            command: {
              id: command.id,
              title: command.title,
              arguments: [m.uri, position, references]
            }
          };
        });

        return { lenses, dispose() {} };
      }
    });
    monaco.languages.registerReferenceProvider('ra-rust', {
      provideReferences(m, pos, { includeDeclaration }) {
        const references = state.references(pos.lineNumber, pos.column, includeDeclaration);
        if (references) {
          return references.map(({ range }: monaco.languages.Location) => ({ uri: m.uri, range }));
        }
      }
    });
    monaco.languages.registerDocumentHighlightProvider('ra-rust', {
      provideDocumentHighlights: (_, pos) => state.references(pos.lineNumber, pos.column, true)
    });
    monaco.languages.registerRenameProvider('ra-rust', {
      provideRenameEdits: (m, pos, newName) => {
        const edits = state.rename(pos.lineNumber, pos.column, newName);
        if (edits) {
          return {
            edits: [
              {
                resource: m.uri,
                edits
              }
            ]
          };
        }
      },
      resolveRenameLocation: (_, pos) => state.prepare_rename(pos.lineNumber, pos.column)
    });
    monaco.languages.registerCompletionItemProvider('ra-rust', {
      triggerCharacters: ['.', ':', '='],
      provideCompletionItems(m, pos) {
        const suggestions = state.completions(pos.lineNumber, pos.column);
        if (suggestions) {
          return { suggestions };
        }
      }
    });
    monaco.languages.registerSignatureHelpProvider('ra-rust', {
      signatureHelpTriggerCharacters: ['(', ','],
      provideSignatureHelp(m, pos) {
        const value = state.signature_help(pos.lineNumber, pos.column);
        if (value) {
          return {
            value,
            dispose() {}
          };
        }
      }
    });
    monaco.languages.registerDefinitionProvider('ra-rust', {
      provideDefinition(m, pos) {
        const list = state.definition(pos.lineNumber, pos.column);
        if (list) {
          return list.map((def: monaco.languages.Definition) => ({ ...def, uri: m.uri }));
        }
      }
    });
    monaco.languages.registerTypeDefinitionProvider('ra-rust', {
      provideTypeDefinition(m, pos) {
        const list = state.type_definition(pos.lineNumber, pos.column);
        if (list) {
          return list.map((def: monaco.languages.Definition) => ({ ...def, uri: m.uri }));
        }
      }
    });
    monaco.languages.registerImplementationProvider('ra-rust', {
      provideImplementation(m, pos) {
        const list = state.goto_implementation(pos.lineNumber, pos.column);
        if (list) {
          return list.map((def: monaco.languages.Definition) => ({ ...def, uri: m.uri }));
        }
      }
    });
    monaco.languages.registerDocumentSymbolProvider('ra-rust', {
      provideDocumentSymbols: () => state.document_symbols()
    });
    monaco.languages.registerOnTypeFormattingEditProvider('ra-rust', {
      autoFormatTriggerCharacters: ['.', '='],
      provideOnTypeFormattingEdits: (_, pos, ch) => state.type_formatting(pos.lineNumber, pos.column, ch)
    });
    monaco.languages.registerFoldingRangeProvider('ra-rust', {
      provideFoldingRanges: () => state.folding_ranges()
    });

    monaco.languages.setTokensProvider('ra-rust', {
      getInitialState: () => new TokenState(),
      tokenize(_, st: TokenState) {
        const filteredTokens = allTokens.filter((token) => token.range.startLineNumber === st.line);

        const tokens = filteredTokens.map((token) => ({
          startIndex: token.range.startColumn - 1,
          scopes: fixTag(token.tag)
        }));
        // add tokens inbetween highlighted ones to remove color artifacts
        tokens.push(
          ...filteredTokens.filter((tok, i) => i === tokens.length - 1 || tokens[i + 1].startIndex > tok.range.endColumn - 1).map((token) => ({
            startIndex: token.range.endColumn - 1,
            scopes: 'operator'
          }))
        );
        tokens.sort((a, b) => a.startIndex - b.startIndex);

        return {
          tokens,
          endState: new TokenState(st.line + 1)
        };
      }
    });
  });
}

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
// @ts-check
import 'monaco-editor/esm/vs/editor/browser/controller/coreCommands';
import 'monaco-editor/esm/vs/editor/browser/widget/codeEditorWidget';
import 'monaco-editor/esm/vs/editor/browser/widget/diffEditorWidget';
import 'monaco-editor/esm/vs/editor/browser/widget/diffNavigator';
import 'monaco-editor/esm/vs/editor/contrib/bracketMatching/bracketMatching';
import 'monaco-editor/esm/vs/editor/contrib/caretOperations/caretOperations';
import 'monaco-editor/esm/vs/editor/contrib/caretOperations/transpose';
import 'monaco-editor/esm/vs/editor/contrib/clipboard/clipboard';
import 'monaco-editor/esm/vs/editor/contrib/codeAction/codeActionContributions';
import 'monaco-editor/esm/vs/editor/contrib/codelens/codelensController';
import 'monaco-editor/esm/vs/editor/contrib/colorPicker/colorDetector';
import 'monaco-editor/esm/vs/editor/contrib/comment/comment';
import 'monaco-editor/esm/vs/editor/contrib/contextmenu/contextmenu';
import 'monaco-editor/esm/vs/editor/contrib/cursorUndo/cursorUndo';
import 'monaco-editor/esm/vs/editor/contrib/dnd/dnd';
import 'monaco-editor/esm/vs/editor/contrib/find/findController';
import 'monaco-editor/esm/vs/editor/contrib/folding/folding';
import 'monaco-editor/esm/vs/editor/contrib/fontZoom/fontZoom';
import 'monaco-editor/esm/vs/editor/contrib/format/formatActions';
import 'monaco-editor/esm/vs/editor/contrib/goToDefinition/goToDefinitionCommands';
import 'monaco-editor/esm/vs/editor/contrib/goToDefinition/goToDefinitionMouse';
import 'monaco-editor/esm/vs/editor/contrib/gotoError/gotoError';
import 'monaco-editor/esm/vs/editor/contrib/hover/hover';
import 'monaco-editor/esm/vs/editor/contrib/inPlaceReplace/inPlaceReplace';
import 'monaco-editor/esm/vs/editor/contrib/linesOperations/linesOperations';
import 'monaco-editor/esm/vs/editor/contrib/links/links';
import 'monaco-editor/esm/vs/editor/contrib/multicursor/multicursor';
import 'monaco-editor/esm/vs/editor/contrib/parameterHints/parameterHints';
import 'monaco-editor/esm/vs/editor/contrib/referenceSearch/referenceSearch';
import 'monaco-editor/esm/vs/editor/contrib/rename/rename';
import 'monaco-editor/esm/vs/editor/contrib/smartSelect/smartSelect';
import 'monaco-editor/esm/vs/editor/contrib/snippet/snippetController2';
import 'monaco-editor/esm/vs/editor/contrib/suggest/suggestController';
import 'monaco-editor/esm/vs/editor/contrib/tokenization/tokenization';
import 'monaco-editor/esm/vs/editor/contrib/toggleTabFocusMode/toggleTabFocusMode';
import 'monaco-editor/esm/vs/editor/contrib/wordHighlighter/wordHighlighter';
import 'monaco-editor/esm/vs/editor/contrib/wordOperations/wordOperations';
import 'monaco-editor/esm/vs/editor/contrib/wordPartOperations/wordPartOperations';
import 'monaco-editor/esm/vs/editor/standalone/browser/accessibilityHelp/accessibilityHelp';
import 'monaco-editor/esm/vs/editor/standalone/browser/iPadShowKeyboard/iPadShowKeyboard';
import 'monaco-editor/esm/vs/editor/standalone/browser/inspectTokens/inspectTokens';
import 'monaco-editor/esm/vs/editor/standalone/browser/quickOpen/gotoLine';
import 'monaco-editor/esm/vs/editor/standalone/browser/quickOpen/quickCommand';
import 'monaco-editor/esm/vs/editor/standalone/browser/quickOpen/quickOutline';
import 'monaco-editor/esm/vs/editor/standalone/browser/referenceSearch/standaloneReferenceSearch';
import 'monaco-editor/esm/vs/editor/standalone/browser/toggleHighContrast/toggleHighContrast';

import * as rustConf from 'monaco-editor/esm/vs/basic-languages/rust/rust';

export interface Token {
  tag: string;
  range: monaco.Range;
}

export default async function registerLanguages() {
  // Rust implementation for hover only
  monaco.languages.register({
    id: 'rust'
  });
  monaco.languages.setMonarchTokensProvider('rust', rustConf.language);

  // Rust analyzer implementation
  monaco.languages.register({
    // language for editor
    id: 'ra-rust',
    extensions: ['.rs', '.rlib'],
    aliases: ['Rust', 'rust']
  });
  monaco.languages.setLanguageConfiguration('ra-rust', rustConf.conf);

  monaco.languages.onLanguage('ra-rust', async () => {
    // state of rust analyzer for wasm codes
    const { WorldState } = await import('wasm-analyzer');

    const state = new WorldState();

    const [model] = monaco.editor.getModels();
    let allTokens: Token[] = [];

    function update() {
      const res = state.update(model.getValue());
      monaco.editor.setModelMarkers(model, 'ra-rust', res.diagnostics);
      allTokens = res.highlights;
    }
    update();

    model.onDidChangeContent(update);

    // monaco.languages.registerHoverProvider('ra-rust', {
    //   provideHover: (_, pos) => state.hover(pos.lineNumber, pos.column)
    // });
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
    // monaco.languages.registerDocumentHighlightProvider('ra-rust', {
    //   provideDocumentHighlights: (_, pos) => {
    //     console.log(state.references(pos.lineNumber, pos.column, true));
    //     return state.references(pos.lineNumber, pos.column, true);
    //   }
    // });
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
        if (!value) return null;
        return {
          value,
          dispose() {}
        };
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

    function fixTag(tag: string) {
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
    }

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

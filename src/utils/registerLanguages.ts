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

import { language, conf } from 'monaco-editor/esm/vs/basic-languages/rust/rust';
import { LanguageUpdater } from './languageUpdater';
import { File, languageForFileType } from '../models';

const languageUpdater = new LanguageUpdater();

// default we will update tokens from all open files for rust
export const updateModelTokens = (file: File) => {
  const { buffer, type } = file;
  const languageId = languageForFileType(type);

  switch (languageId) {
    case 'rust':
      languageUpdater.addModel(buffer, languageId);
      break;
    default:
      break;
  }
};

export default async function registerLanguages() {
  // Log
  monaco.languages.onLanguage('log', () => {
    monaco.languages.setMonarchTokensProvider('log', {
      tokenizer: {
        root: [[/error[:[].*/, 'custom-error'], [/warn[:[].*/, 'custom-warn'], [/notice[:[].*/, 'custom-notice'], [/info[:[].*/, 'custom-info']]
      }
    });
  });
  monaco.languages.register({
    id: 'log'
  });

  // rust languages
  monaco.languages.register({
    id: 'rust'
  });
  monaco.languages.setMonarchTokensProvider('rust', language);
  monaco.languages.setLanguageConfiguration('rust', conf);

  monaco.languages.registerHoverProvider('rust', {
    provideHover(model, pos) {
      return languageUpdater.provideHover(model, pos);
    }
  });
  monaco.languages.registerCodeLensProvider('rust', {
    provideCodeLenses(model) {
      return languageUpdater.provideCodeLenses(model);
    }
  });
  monaco.languages.registerReferenceProvider('rust', {
    provideReferences(model, pos, context) {
      return languageUpdater.provideReferences(model, pos, context);
    }
  });
  monaco.languages.registerDocumentHighlightProvider('rust', {
    provideDocumentHighlights(model, pos) {
      return languageUpdater.provideDocumentHighlights(model, pos);
    }
  });
  monaco.languages.registerRenameProvider('rust', {
    provideRenameEdits: (model, pos, newName) => {
      return languageUpdater.provideRenameEdits(model, pos, newName);
    },
    resolveRenameLocation(model, pos) {
      return languageUpdater.resolveRenameLocation(model, pos);
    }
  });
  monaco.languages.registerCompletionItemProvider('rust', {
    triggerCharacters: ['.', ':', '='],
    provideCompletionItems(model, pos) {
      return languageUpdater.provideCompletionItems(model, pos);
    }
  });
  monaco.languages.registerSignatureHelpProvider('rust', {
    signatureHelpTriggerCharacters: ['(', ','],
    provideSignatureHelp(model, pos) {
      return languageUpdater.provideSignatureHelp(model, pos);
    }
  });
  monaco.languages.registerDefinitionProvider('rust', {
    provideDefinition(model, pos) {
      return languageUpdater.provideDefinition(model, pos);
    }
  });
  monaco.languages.registerTypeDefinitionProvider('rust', {
    provideTypeDefinition(model, pos) {
      return languageUpdater.provideTypeDefinition(model, pos);
    }
  });
  monaco.languages.registerImplementationProvider('rust', {
    provideImplementation(model, pos) {
      return languageUpdater.provideImplementation(model, pos);
    }
  });
  monaco.languages.registerDocumentSymbolProvider('rust', {
    provideDocumentSymbols(model) {
      return languageUpdater.provideDocumentSymbols(model);
    }
  });
  monaco.languages.registerOnTypeFormattingEditProvider('rust', {
    autoFormatTriggerCharacters: ['.', '='],
    provideOnTypeFormattingEdits(model, pos, ch) {
      return languageUpdater.provideOnTypeFormattingEdits(model, pos, ch);
    }
  });
  monaco.languages.registerFoldingRangeProvider('rust', {
    provideFoldingRanges(model) {
      return languageUpdater.provideFoldingRanges(model);
    }
  });
}

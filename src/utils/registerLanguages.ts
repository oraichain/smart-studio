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

import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';
import { Service } from '../service';
import { Language } from '../compilerServices/types';
import { language, conf } from './rust-grammar';

export default async function registerLanguages() {
  // Log
  monaco.languages.onLanguage(Language.Log, () => {
    monaco.languages.setMonarchTokensProvider(Language.Log, {
      tokenizer: {
        root: [[/error[:[].*/, 'custom-error'], [/warn[:[].*/, 'custom-warn'], [/notice[:[].*/, 'custom-notice'], [/info[:[].*/, 'custom-info']]
      }
    });
  });
  monaco.languages.register({
    id: Language.Log
  });

  // rust languages
  monaco.languages.register({
    id: Language.Rust
  });

  monaco.languages.setMonarchTokensProvider(Language.Rust, language);
  monaco.languages.setLanguageConfiguration(Language.Rust, conf);

  monaco.languages.registerHoverProvider(Language.Rust, {
    provideHover(model, pos) {
      return Service.LanguageUpdater.provideHover(model, pos);
    }
  });
  monaco.languages.registerCodeLensProvider(Language.Rust, {
    provideCodeLenses(model) {
      return Service.LanguageUpdater.provideCodeLenses(model);
    }
  });
  monaco.languages.registerReferenceProvider(Language.Rust, {
    provideReferences(model, pos, context) {
      return Service.LanguageUpdater.provideReferences(model, pos, context);
    }
  });
  monaco.languages.registerDocumentHighlightProvider(Language.Rust, {
    provideDocumentHighlights(model, pos) {
      return Service.LanguageUpdater.provideDocumentHighlights(model, pos);
    }
  });
  monaco.languages.registerRenameProvider(Language.Rust, {
    provideRenameEdits: (model, pos, newName) => {
      return Service.LanguageUpdater.provideRenameEdits(model, pos, newName);
    },
    resolveRenameLocation(model, pos) {
      return Service.LanguageUpdater.resolveRenameLocation(model, pos);
    }
  });
  monaco.languages.registerCompletionItemProvider(Language.Rust, {
    triggerCharacters: ['.', ':', '='],
    provideCompletionItems(model, pos) {
      return Service.LanguageUpdater.provideCompletionItems(model, pos);
    }
  });
  // monaco.languages.registerInlayHintsProvider(Language.Rust, {
  //   provideInlayHints(model, range, token) {
  //     return Service.LanguageUpdater.provideInlayHints(model, range, token);
  //   }
  // });

  monaco.languages.registerSignatureHelpProvider(Language.Rust, {
    signatureHelpTriggerCharacters: ['(', ','],
    provideSignatureHelp(model, pos) {
      return Service.LanguageUpdater.provideSignatureHelp(model, pos);
    }
  });
  monaco.languages.registerDefinitionProvider(Language.Rust, {
    provideDefinition(model, pos) {
      return Service.LanguageUpdater.provideDefinition(model, pos);
    }
  });
  monaco.languages.registerTypeDefinitionProvider(Language.Rust, {
    provideTypeDefinition(model, pos) {
      return Service.LanguageUpdater.provideTypeDefinition(model, pos);
    }
  });
  monaco.languages.registerImplementationProvider(Language.Rust, {
    provideImplementation(model, pos) {
      return Service.LanguageUpdater.provideImplementation(model, pos);
    }
  });
  monaco.languages.registerDocumentSymbolProvider(Language.Rust, {
    provideDocumentSymbols(model) {
      return Service.LanguageUpdater.provideDocumentSymbols(model);
    }
  });
  monaco.languages.registerOnTypeFormattingEditProvider(Language.Rust, {
    autoFormatTriggerCharacters: ['.', '='],
    provideOnTypeFormattingEdits(model, pos, ch) {
      return Service.LanguageUpdater.provideOnTypeFormattingEdits(model, pos, ch);
    }
  });
  monaco.languages.registerFoldingRangeProvider(Language.Rust, {
    provideFoldingRanges(model) {
      return Service.LanguageUpdater.provideFoldingRanges(model);
    }
  });
}

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

import { language, conf } from './rust-grammar';
import { Language } from '../compilerServices/types';
import { LanguageUpdater } from './languageUpdater';

export default function registerLanguages() {
  // Log
  monaco.languages.register({
    id: Language.Log
  });
  monaco.languages.setMonarchTokensProvider(Language.Log, {
    tokenizer: {
      root: [[/error[:[].*/, 'custom-error'], [/warn[:[].*/, 'custom-warn'], [/notice[:[].*/, 'custom-notice'], [/info[:[].*/, 'custom-info']]
    }
  });

  // rust languages
  monaco.languages.register({
    id: Language.Rust
  });

  monaco.languages.setMonarchTokensProvider(Language.Rust, language);
  monaco.languages.setLanguageConfiguration(Language.Rust, conf);

  const raLanguage = LanguageUpdater.instance.AnalyzerLanguageId;
  if (raLanguage !== Language.Rust) {
    // rust analyzer language for editor
    monaco.languages.register({
      id: raLanguage
    });
    monaco.languages.setLanguageConfiguration(raLanguage, conf);
  }

  monaco.languages.registerHoverProvider(raLanguage, {
    provideHover(model, pos) {
      return LanguageUpdater.instance.provideHover(model, pos);
    }
  });

  monaco.languages.registerCodeLensProvider(raLanguage, {
    provideCodeLenses(model) {
      return LanguageUpdater.instance.provideCodeLenses(model);
    }
  });

  monaco.languages.registerReferenceProvider(raLanguage, {
    provideReferences(model, pos, context) {
      return LanguageUpdater.instance.provideReferences(model, pos, context);
    }
  });

  monaco.languages.registerDocumentHighlightProvider(raLanguage, {
    provideDocumentHighlights(model, pos) {
      return LanguageUpdater.instance.provideDocumentHighlights(model, pos);
    }
  });

  monaco.languages.registerRenameProvider(raLanguage, {
    provideRenameEdits: (model, pos, newName) => {
      return LanguageUpdater.instance.provideRenameEdits(model, pos, newName);
    },
    resolveRenameLocation(model, pos) {
      return LanguageUpdater.instance.resolveRenameLocation(model, pos);
    }
  });

  monaco.languages.registerCompletionItemProvider(raLanguage, {
    triggerCharacters: ['.', ':', '='],
    provideCompletionItems(model, pos) {
      return LanguageUpdater.instance.provideCompletionItems(model, pos);
    }
  });

  monaco.languages.registerInlayHintsProvider(raLanguage, {
    provideInlayHints(model: any) {
      return LanguageUpdater.instance.provideInlayHints(model);
    }
  });

  monaco.languages.registerSignatureHelpProvider(raLanguage, {
    signatureHelpTriggerCharacters: ['(', ','],
    provideSignatureHelp(model, pos) {
      return LanguageUpdater.instance.provideSignatureHelp(model, pos);
    }
  });

  monaco.languages.registerDefinitionProvider(raLanguage, {
    provideDefinition(model, pos) {
      return LanguageUpdater.instance.provideDefinition(model, pos);
    }
  });

  monaco.languages.registerTypeDefinitionProvider(raLanguage, {
    provideTypeDefinition(model, pos) {
      return LanguageUpdater.instance.provideTypeDefinition(model, pos);
    }
  });

  monaco.languages.registerImplementationProvider(raLanguage, {
    provideImplementation(model, pos) {
      return LanguageUpdater.instance.provideImplementation(model, pos);
    }
  });

  monaco.languages.registerDocumentSymbolProvider(raLanguage, {
    provideDocumentSymbols(model) {
      return LanguageUpdater.instance.provideDocumentSymbols(model);
    }
  });

  monaco.languages.registerOnTypeFormattingEditProvider(raLanguage, {
    autoFormatTriggerCharacters: ['.', '='],
    provideOnTypeFormattingEdits(model, pos, ch) {
      return LanguageUpdater.instance.provideOnTypeFormattingEdits(model, pos, ch);
    }
  });

  monaco.languages.registerFoldingRangeProvider(raLanguage, {
    provideFoldingRanges(model) {
      return LanguageUpdater.instance.provideFoldingRanges(model);
    }
  });
}

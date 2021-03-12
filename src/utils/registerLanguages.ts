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

import { Rust } from '../languages/rust';
import { Toml } from '../languages/toml';

export default async function registerLanguages() {
  // toml

  monaco.languages.onLanguage('toml', () => {
    monaco.languages.setMonarchTokensProvider('toml', Toml.MonarchDefinitions);
    monaco.languages.registerCompletionItemProvider('toml', Toml.CompletionItemProvider);
  });
  monaco.languages.register({
    id: 'toml'
  });

  // Rust

  monaco.languages.onLanguage('rust', () => {
    monaco.languages.setMonarchTokensProvider('rust', Rust.MonarchDefinitions);
    monaco.languages.registerCompletionItemProvider('rust', Rust.CompletionItemProvider);
  });
  monaco.languages.register({
    id: 'rust'
  });
}

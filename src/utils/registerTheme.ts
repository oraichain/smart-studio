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

const dark_teal = '67c6b0';
const dark_text = 'ffffff';
const dark_green = '709950';
const dark_orange = 'ce9178';
const dark_blue = '6298d4';
const dark_blue_secondary = '95d9fc';
const dark_yellow = 'dcdd9b';

const light_teal = '14b79d';
const light_text = '000000';
const light_green = '008000';
const light_orange = 'a51b1b';
const light_blue = '0000ff';
const light_blue_secondary = '0A59DF';
const light_yellow = 'D9CA00';

export default function registerTheme() {
  monaco.editor.defineTheme('fiddle-theme', {
    base: 'vs-dark',
    inherit: true,
    colors: {},
    rules: [
      { token: 'custom-info', foreground: 'd4d4d4' },
      { token: 'custom-warn', foreground: 'ff9900' },
      { token: 'custom-error', background: '00ff00', foreground: 'ff0000', fontStyle: 'bold' },
      { token: 'comment', foreground: dark_green },
      { token: 'builtin_attr', foreground: dark_teal },
      { token: 'string_literal', foreground: dark_orange },
      { token: 'keyword', foreground: dark_blue },
      { token: 'module', foreground: dark_teal },
      { token: 'struct', foreground: dark_teal },
      { token: 'field', foreground: dark_blue_secondary },
      { token: 'builtin_type', foreground: dark_teal },
      { token: 'function', foreground: dark_yellow },
      { token: 'value_param', foreground: dark_teal },
      { token: 'self_type', foreground: dark_teal },
      { token: 'trait', foreground: dark_teal },
      { token: 'self_keyword', foreground: dark_blue },
      { token: 'variable', foreground: dark_blue_secondary },
      { token: 'unresolved_reference', foreground: dark_blue },
      { token: 'macro', foreground: dark_blue },
      { token: 'bool_literal', foreground: dark_blue_secondary },
      { token: 'none', foreground: dark_text },
      { token: 'attribute', foreground: dark_text },
      { token: 'parenthesis', foreground: dark_text },
      { token: 'operator', foreground: dark_text },
      { token: 'comma', foreground: dark_text },
      { token: 'semicolon', foreground: dark_text },
      { token: 'brace', foreground: dark_text },
      { token: 'colon', foreground: dark_text },
      { token: 'logical', foreground: dark_text },
      { token: 'punctuation', foreground: dark_text }
    ]
  });
  monaco.editor.setTheme('fiddle-theme');
}

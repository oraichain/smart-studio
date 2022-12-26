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
import { CompilerService, ServiceInput, ServiceOutput } from './types';
import { sendRequest, ServiceTypes } from './sendRequest';
import { encodeBinary } from './utils';

export class X86Service implements CompilerService {
  async compile(input: ServiceInput): Promise<ServiceOutput> {
    const files = Object.values(input.files);
    if (files.length !== 1) {
      throw new Error(`Supporting compilation of a single file, but ${files.length} file(s) found`);
    }
    const wasm = files[0].content as ArrayBuffer;
    const options = input.options;
    const encodedWasm = encodeURIComponent(encodeBinary(wasm));
    const content: any = await sendRequest('input=' + encodedWasm + '&action=wasm2assembly&options=' + encodeURIComponent(options), ServiceTypes.Rustc);
    return {
      success: true,
      items: {
        'a.json': { content }
      }
    };
  }
}

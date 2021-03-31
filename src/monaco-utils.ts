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
import { Action } from 'monaco-editor/esm/vs/base/common/actions';
import { ContextSubMenu } from 'monaco-editor/esm/vs/base/browser/contextmenu';
import { ContextViewService } from 'monaco-editor/esm/vs/platform/contextview/browser/contextViewService';
import { ContextMenuService } from 'monaco-editor/esm/vs/platform/contextview/browser/contextMenuService';

import { Tree } from 'monaco-editor/esm/vs/base/parts/tree/browser/treeImpl';

import { ITree } from './monaco-extra';
// Utils provided by monaco editor, but exposed only via AMD require().
// See index.tsx for initialization.
export class MonacoUtils {
  static Tree = Tree;
  static ContextSubMenu = ContextSubMenu;
  static ContextMenuService = ContextMenuService;
  static ContextViewService = ContextViewService;
  static Action = Action;

  static expandTree(tree: ITree) {
    const model = tree.model;
    const elements = [];

    let item;
    const nav = model.getNavigator();

    while ((item = nav.next())) {
      elements.push(item);
    }

    for (let element of elements) {
      model.expand(element);
    }
  }
}

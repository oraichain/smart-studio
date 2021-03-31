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

import { DefaultController } from 'monaco-editor/esm/vs/base/parts/tree/browser/treeDefaults';
import { MonacoUtils } from './monaco-utils';
import { ITree, ContextMenuEvent } from './monaco-extra';

export class MonacoController extends DefaultController {
  private getActionsFn?: Function;

  constructor(getActionsFn?: Function) {
    super();
    this.getActionsFn = getActionsFn;
  }

  onContextMenu(tree: ITree, file: File, event: ContextMenuEvent): boolean {
    // default Project and nothing
    if (file.name === 'Project') return;
    tree.setFocus(file);
    const anchorOffset = { x: -10, y: -3 };
    const anchor = { x: event._posx + anchorOffset.x, y: event._posy + anchorOffset.y };
    const actions = this.getActionsFn && this.getActionsFn(file, event);

    if (!actions || !actions.length) {
      return false;
    }

    MonacoUtils.ContextMenuserviceInstance.showContextMenu({
      getAnchor: () => anchor,
      getActions: () => actions,
      getActionViewItem: (action: any): any => null,
      onHide: (wasCancelled?: boolean) => {
        if (wasCancelled) {
          tree.domFocus();
        }
      }
    });

    super.onContextMenu(tree, file, event);

    return true;
  }
}

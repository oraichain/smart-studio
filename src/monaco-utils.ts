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
// import { Action } from 'monaco-editor/esm/vs/base/common/actions';
// import { ContextSubMenu } from 'monaco-editor/esm/vs/base/browser/contextmenu';
// import { ContextMenuService } from 'monaco-editor/esm/vs/platform/contextview/browser/contextMenuService';
// import { StandaloneServices } from 'monaco-editor/esm/vs/editor/standalone/browser/standaloneServices';
// import { IContextViewService } from 'monaco-editor/esm/vs/platform/contextview/browser/contextView';
// import { ITelemetryService } from 'monaco-editor/esm/vs/platform/telemetry/common/telemetry.js';
// import { IThemeService } from 'monaco-editor/esm/vs/platform/theme/common/themeService.js';
// import { IKeybindingService } from 'monaco-editor/esm/vs/platform/keybinding/common/keybinding.js';
// import { INotificationService } from 'monaco-editor/esm/vs/platform/notification/common/notification.js';
// import { Tree } from 'monaco-editor/esm/vs/base/parts/tree/browser/treeImpl';

// import { ITree } from './monaco-extra';
import registerTheme from './utils/registerTheme';
import registerLanguages from './utils/registerLanguages';
// Utils provided by monaco editor, but exposed only via AMD require().
// See index.tsx for initialization.
export class MonacoUtils {
  // static Tree = Tree;
  // static ContextSubMenu = ContextSubMenu;
  // static Action = Action;
  // static ContextMenuserviceInstance: any;
  static initialize() {
    // define theme
    registerTheme();

    // languages
    registerLanguages();

    // // define services
    // const services = new StandaloneServices(document.documentElement, {});
    // const telemetryService = services.get(ITelemetryService);
    // const notificationService = services.get(INotificationService);
    // const contextViewService = services.get(IContextViewService);
    // const keybindingService = services.get(IKeybindingService);

    // // set fiddle-theme
    // const themeService = services.get(IThemeService);
    // themeService.setTheme('fiddle-theme');

    // // create static ContextMenuserviceInstance
    // MonacoUtils.ContextMenuserviceInstance = new ContextMenuService(telemetryService, notificationService, contextViewService, keybindingService, themeService);
  }

  // static expandTree(tree: ITree) {
  //   const model = tree.model;
  //   const elements = [];

  //   let item;
  //   const nav = model.getNavigator();

  //   while ((item = nav.next())) {
  //     elements.push(item);
  //   }

  //   for (let element of elements) {
  //     model.expand(element);
  //   }
  // }
}

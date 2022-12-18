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
// import { ContextSubMenu } from 'monaco-editor/esm/vs/base/browser/contextmenu';
import { ContextMenuService } from 'monaco-editor/esm/vs/platform/contextview/browser/contextMenuService';
import { StandaloneServices } from 'monaco-editor/esm/vs/editor/standalone/browser/standaloneServices';
import { IContextViewService } from 'monaco-editor/esm/vs/platform/contextview/browser/contextView';
import { ITelemetryService } from 'monaco-editor/esm/vs/platform/telemetry/common/telemetry';
import { IThemeService } from 'monaco-editor/esm/vs/platform/theme/common/themeService';
import { ICodeEditorService } from 'monaco-editor/esm/vs/editor/browser/services/codeEditorService';
import { IKeybindingService } from 'monaco-editor/esm/vs/platform/keybinding/common/keybinding.js';
import { INotificationService } from 'monaco-editor/esm/vs/platform/notification/common/notification.js';

import { ITree } from './monaco-extra';
import registerTheme from './utils/registerTheme';
import registerLanguages from './utils/registerLanguages';
import appStore from './stores/AppStore';
import { openFile } from './actions/AppActions';
import { LanguageUpdater } from './utils/languageUpdater';
// Utils provided by monaco editor, but exposed only via AMD require().
// See index.tsx for initialization.
export class MonacoUtils {
  // static ContextSubMenu = ContextSubMenu;
  static Action = Action;
  static ContextMenuserviceInstance: any;
  static initialize() {
    // define theme
    registerTheme();

    // languages
    registerLanguages();

    // define services
    const telemetryService = StandaloneServices.get(ITelemetryService);
    const notificationService = StandaloneServices.get(INotificationService);
    const contextViewService = StandaloneServices.get(IContextViewService);
    const keybindingService = StandaloneServices.get(IKeybindingService);

    // set fiddle-theme
    const themeService = StandaloneServices.get(IThemeService);
    themeService.setTheme('fiddle-theme');

    const codeEditorService = StandaloneServices.get(ICodeEditorService);
    const openEditorBase = codeEditorService.openCodeEditor.bind(codeEditorService);
    codeEditorService.openCodeEditor = async (input: { resource: monaco.Uri; options: { selection: monaco.Range } }, source: any) => {
      const result = await openEditorBase(input, source);
      if (result === null) {
        const path = LanguageUpdater.instance.getPath(input.resource);

        const project = appStore.getProject().getModel();
        const file = project.getFile(path);
        const range = input.options.selection;
        const position = { lineNumber: range.startLineNumber, column: range.startColumn };
        // open this file
        openFile(file, undefined, undefined, position);
      }

      return result; // always return the base result
    };

    // create static ContextMenuserviceInstance
    MonacoUtils.ContextMenuserviceInstance = new ContextMenuService(telemetryService, notificationService, contextViewService, keybindingService, themeService);
  }

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

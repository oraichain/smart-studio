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

import React from 'react';
import appStore from '../stores/AppStore';
import { File, Directory, Problem, Project } from '../models';
import { ITree } from '../monaco-extra';
import { ProblemTemplate, FileTemplate } from '../utils/Template';
import { MonacoUtils } from '../monaco-utils';
import { openFile } from '../actions/AppActions';
import { ViewType } from './editor/View';
import { createController } from '../monaco-controller';

export interface ProblemsProps {}

export class Problems extends React.Component<ProblemsProps, {}> {
  tree: ITree;
  container: HTMLDivElement;

  constructor(props: ProblemsProps) {
    super(props);
    // tslint:disable-next-line
  }

  componentDidMount() {
    this.ensureTree();
    this.tree.model.setInput(appStore.getProject().getModel());
    this.tree.model.onDidSelect((e: any) => {
      if (e.selection.length) {
        this.onSelectProblem(e.selection[0]);
      }
    });
    appStore.onDidChangeProblems.register(() => {
      this.tree.model.refresh();
      MonacoUtils.expandTree(this.tree);
    });
    appStore.onLoadProject.register(() => {
      this.tree.model.setInput(appStore.getProject().getModel());
    });
  }
  componentWillReceiveProps(props: ProblemsProps) {
    this.tree.model.refresh();
    MonacoUtils.expandTree(this.tree);
  }
  private setContainer(container: HTMLDivElement) {
    if (container == null) {
      return;
    }
    this.container = container;
  }
  private ensureTree() {
    if (this.container.lastChild) {
      this.container.removeChild(this.container.lastChild);
    }
    this.tree = new MonacoUtils.Tree(this.container, {
      dataSource: {
        /**
         * Returns the unique identifier of the given element.
         * No more than one element may use a given identifier.
         */
        getId: function(tree: ITree, element: File): string {
          return element.key;
        },

        /**
         * Returns a boolean value indicating whether the element has children.
         */
        hasChildren: function(tree: ITree, element: File | Problem): boolean {
          if (element instanceof Directory && element.children.length) {
            return true;
          } else if (element instanceof File) {
            return element.problems.length > 0;
          }
          return false;
        },

        /**
         * Returns the element's children as an array in a promise.
         */
        getChildren: function(tree: ITree, element: File | Problem): Promise<File[] | Problem[]> {
          if (element instanceof Directory && element.children.length) {
            const children: File[] = [];
            element.forEachFile(
              (file: File) => {
                if (file.problems.length) {
                  children.push(file);
                }
              },
              false,
              true
            );
            return Promise.resolve(children);
          } else if (element instanceof File) {
            return Promise.resolve(element.problems);
          }
          return Promise.resolve(null);
        },

        /**
         * Returns the element's parent in a promise.
         */
        getParent: function(tree: ITree, element: File | Problem): Promise<File | Project> {
          if (element instanceof File) {
            return Promise.resolve(element.getProject());
          }
          return Promise.resolve(element.file);
        }
      },
      renderer: {
        getHeight: function(tree: ITree, element: File): number {
          return 24;
        },
        getTemplateId(tree: ITree, element: File | Problem): string {
          if (element instanceof File) {
            return 'file';
          }
          return 'problem';
        },
        renderTemplate: function(tree: ITree, templateId: string, container: any): any {
          return templateId === 'problem' ? new ProblemTemplate(container) : new FileTemplate(container);
        },
        renderElement: function(tree: ITree, element: File | Problem, templateId: string, templateData: any): void {
          templateData.set(element);
        },
        disposeTemplate: function(tree: ITree, templateId: string, templateData: any): void {
          templateData.dispose();
        }
      },
      controller: createController(this)
    });
  }
  onSelectProblem(problem: File | Problem) {
    if (problem instanceof File) {
      return;
    }
    openFile(problem.file, ViewType.Editor, true);
  }
  render() {
    return <div className="fill" ref={(ref) => this.setContainer(ref)} />;
  }
}

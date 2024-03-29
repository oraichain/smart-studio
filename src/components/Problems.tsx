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
    });
    appStore.onLoadProject.register(() => {
      this.tree.model.setInput(appStore.getProject().getModel());
    });
  }
  componentWillReceiveProps(props: ProblemsProps) {
    this.tree.model.refresh();
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

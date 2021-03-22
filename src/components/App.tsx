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
import { hot } from 'react-hot-loader/root';

import { Workspace } from './Workspace';
import { ViewTabs, View } from './editor';
import { Toolbar } from './Toolbar';
import { ViewType, defaultViewTypeForFileType } from './editor/View';
import { schema, build, test, run, runTask, openFiles, pushStatus, popStatus, openProject } from '../actions/AppActions';

import appStore from '../stores/AppStore';
import {
  addFileTo,
  loadProject,
  initStore,
  updateFileNameAndDescription,
  deleteFile,
  splitGroup,  
  openFile,
  openView,
  closeView,
  closeTabs,
  saveProject,
  focusTabGroup,
  setViewType,
  logLn
} from '../actions/AppActions';
import { Project, File, FileType, Directory, ModelRef } from '../models';
import { Service, Language } from '../service';
import { Split, SplitOrientation, SplitInfo } from './Split';

import { layout, assert, resetDOMSelection } from '../util';

import Mousetrap from 'mousetrap';
import { GoDelete, GoDesktopDownload, GoBeaker, GoThreeBars, GoQuestion, GoVerified, GoCheck, GoBeakerGear, GoKebabHorizontal } from './shared/Icons';
import { Button } from './shared/Button';

import { NewFileDialog } from './NewFileDialog';
import { EditFileDialog } from './EditFileDialog';
import { UploadFileDialog } from './UploadFileDialog';
import { ToastContainer, ToastKind } from './Toasts';
import { Spacer, Divider } from './Widgets';
import { ShareDialog } from './ShareDialog';
import { NewProjectDialog, Template } from './NewProjectDialog';
import { NewDirectoryDialog } from './NewDirectoryDialog';
import { Errors } from '../errors';
import { ControlCenter } from './ControlCenter';
import Group from '../utils/group';
import { StatusBar } from './StatusBar';
import { RunTaskExternals } from '../utils/taskRunner';

export interface AppState {
  project: ModelRef<Project>;
  file: ModelRef<File>;
  fiddle: string;

  /**
   * If not null, the the new file dialog is open and files are created in this
   * directory.
   */
  newFileDialogDirectory?: ModelRef<Directory>;

  /**
   * If not null, the the edit file dialog is open.
   */
  editFileDialogFile?: ModelRef<File>;

  /**
   * If true, the share fiddle dialog is open.
   */
  shareDialog: boolean;

  /**
   * If true, the new project dialog is open.
   */
  newProjectDialog: boolean;

  /**
   * Primary workspace split state.
   */
  workspaceSplits: SplitInfo[];

  /**
   * Secondary control center split state.
   */
  controlCenterSplits: SplitInfo[];

  /**
   * Editor split state.
   */
  editorSplits: SplitInfo[];
  /**
   * If not null, the upload file dialog is open.
   */
  uploadFileDialogDirectory: ModelRef<Directory>;
  /**
   * If true, the new directory dialog is open.
   */
  newDirectoryDialog: ModelRef<Directory>;
  showProblems: boolean;
  showSandbox: boolean;
  tabGroups: Group[];
  activeTabGroup: Group;
  hasStatus: boolean;
  isContentModified: boolean;
  windowDimensions: string;
}

export interface AppProps {
  /**
   * If true, the Update button is visible.
   */
  update: boolean;
  keystation: any;
  fiddle: string;
  embeddingParams: EmbeddingParams;
  windowContext: AppWindowContext;
}

export enum EmbeddingType {
  None,
  Default,
  Arc
}

export interface EmbeddingParams {
  type: EmbeddingType;
  templatesName: string;
}

export interface AppWindowContext {
  promptWhenClosing: boolean;
}

@hot
export class App extends React.Component<AppProps, AppState> {
  fiddle: string;
  toastContainer: ToastContainer;
  constructor(props: AppProps) {
    super(props);
    this.state = {
      fiddle: props.fiddle,
      project: null,
      file: null,
      newFileDialogDirectory: null,
      editFileDialogFile: null,
      newProjectDialog: !props.fiddle,
      shareDialog: false,
      workspaceSplits: [
        {
          min: 200,
          max: 400,
          value: 200
        },
        {
          min: 256
        }
      ],
      controlCenterSplits: [{ min: 100 }, { min: 40, value: 256 }],
      editorSplits: [],
      showProblems: true,
      showSandbox: props.embeddingParams.type !== EmbeddingType.Arc,
      uploadFileDialogDirectory: null,
      newDirectoryDialog: null,
      tabGroups: null,
      activeTabGroup: null,
      windowDimensions: App.getWindowDimensions(),
      hasStatus: false,
      isContentModified: false
    };
  }
  private async initializeProject() {
    initStore();
    this.setState({
      project: appStore.getProject(),
      tabGroups: appStore.getTabGroups(),
      activeTabGroup: appStore.getActiveTabGroup(),
      hasStatus: appStore.hasStatus()
    });
    this.bindAppStoreEvents();
    if (this.state.fiddle) {
      this.loadProjectFromFiddle(this.state.fiddle);
    }
  }

  private static getWindowDimensions(): string {
    return `${window.innerWidth}x${window.innerHeight}@${window.devicePixelRatio}`;
  }

  private showToast(element: JSX.Element, kind?: ToastKind) {
    if (this.toastContainer) {
      this.toastContainer.showToast(element, kind);
    }
  }

  private async loadProjectFromFiddle(uri: string) {
    const project = new Project(uri);

    pushStatus('Downloading Project');
    const fiddle = await Service.loadJSON(uri);
    popStatus();
    if (fiddle.success) {
      await Service.loadFilesIntoProject(fiddle.files, project);
      loadProject(project);
      if (project.getFile('README.md')) {
        openFiles([['README.md']]);
        logLn(`Load project ${project.name} succeed!`);
      }
    } else {
      this.showToast(<span>Project {uri} was not found.</span>, 'error');
    }
  }
  bindAppStoreEvents() {
    appStore.onLoadProject.register(() => {
      this.setState({ project: appStore.getProject() });
      runTask('project:load', true, RunTaskExternals.Setup);
    });
    appStore.onDirtyFileUsed.register((file: File) => {
      logLn(`Changes in ${file.getPath()} were ignored, save your changes.`, 'warn');
    });
    appStore.onTabsChange.register(() => {
      this.setState({
        tabGroups: appStore.getTabGroups(),
        activeTabGroup: appStore.getActiveTabGroup()
      });
      layout();
    });
    appStore.onDidChangeStatus.register(() => {
      this.setState({
        hasStatus: appStore.hasStatus()
      });
    });
    appStore.onDidChangeIsContentModified.register(() => {
      this.props.windowContext.promptWhenClosing = appStore.getIsContentModified();

      this.setState({
        isContentModified: appStore.getIsContentModified()
      });
    });
  }

  // TODO: Optimize
  // shouldComponentUpdate(nextProps: any, nextState: AppState) {
  //   let state = this.state;
  //   if (state.file !== nextState.file) return true;
  //   if (state.group !== nextState.group) return true;
  //   if (!shallowCompare(state.groups, nextState.groups)) return true;
  //   return false;
  // }

  // async loadReleaseNotes() {
  //   const response = await fetch('notes/notes.md');
  //   const src = await response.text();
  //   const notes = new File('Release Notes', FileType.Markdown);
  //   notes.setData(src);
  //   openFile(notes, defaultViewTypeForFileType(notes.type));
  // }

  async loadHelp() {
    const response = await fetch('notes/help.md');
    const src = await response.text();
    const help = new File('Help', FileType.Markdown);
    help.setData(src);
    openFile(help, defaultViewTypeForFileType(help.type));
  }

  registerShortcuts() {
    Mousetrap.bind('command+b', () => {
      build();
    });
    Mousetrap.bind('command+enter', () => {
      if (this.props.embeddingParams.type !== EmbeddingType.Arc) {
        run();
      }
    });
    Mousetrap.bind('command+alt+enter', () => {
      if (this.props.embeddingParams.type !== EmbeddingType.Arc) {
        build().then(run);
      }
    });
  }

  componentWillMount() {
    this.initializeProject();
  }

  componentDidMount() {
    layout();
    this.registerShortcuts();
    window.addEventListener(
      'resize',
      () => {
        this.setState({
          windowDimensions: App.getWindowDimensions()
        });
      },
      false
    );
    if (this.props.embeddingParams.type === EmbeddingType.Arc) {
      window.addEventListener('message', (e) => {
        // TODO: background message
        console.log(e);
      });
    }
  }

  share() {
    this.setState({ shareDialog: true });
  }

  // deploy contract using wallet
  async deployContract(file?: File) {
    pushStatus('Deploying Smart Contract');
    const target: File = file || this.state.project.getModel();
    const ret = await Service.exportToWallet(target);
    popStatus();
    if (ret.data) {      
      this.props.keystation?.deploy({ name: file.name, data: ret.data, size: ret.data.length });
    }
  }

  async download() {
    logLn('Downloading Project ...');
    const downloadService = await import('../utils/download');
    const projectModel = this.state.project.getModel();
    await downloadService.downloadProject(projectModel, this.state.fiddle);
    logLn('Project Zip CREATED ');
  }

  /**
   * Remember workspace split.
   */
  private workspaceSplit: SplitInfo = null;

  toolbarButtonsAreDisabled() {
    return this.state.hasStatus;
  }

  makeToolbarButtons() {
    const toolbarButtons = [
      <Button
        key="ViewWorkspace"
        icon={<GoThreeBars />}
        title="View Project Workspace"
        onClick={() => {
          const workspaceSplits = this.state.workspaceSplits;
          const first = workspaceSplits[0];
          const second = workspaceSplits[1];
          if (this.workspaceSplit) {
            Object.assign(first, this.workspaceSplit);
            this.workspaceSplit = null;
            delete second.value;
          } else {
            this.workspaceSplit = Object.assign({}, first);
            first.max = first.min = 0;
          }
          this.setState({ workspaceSplits });
        }}
      />
    ];

    if (this.props.embeddingParams.type === EmbeddingType.None) {
      toolbarButtons.push(
        <Button
          key="DeleteProject"
          icon={<GoDelete />}
          label="Delete"
          title="Delete Project"
          isDisabled={this.toolbarButtonsAreDisabled()}
          onClick={async () => {
            const message = `Are you sure you want to delete Project '${this.state.fiddle}' and its contents?`;

            if (confirm(message)) {
              const projectModel = this.state.project.getModel();
              const ret = await Service.deleteFile(projectModel);
              if (ret.success) {
                location.search = '';
              } else {
                this.showToast(<span>${ret.message}</span>, 'error');
              }
            }
          }}
        />,
        <Button
          key="Download"
          icon={<GoDesktopDownload />}
          label="Download"
          title="Download Project"
          isDisabled={this.toolbarButtonsAreDisabled()}
          onClick={() => {
            this.download();
          }}
        />
      );
    }
    toolbarButtons.push(
      <Button
        key="Build"
        icon={<GoBeaker />}
        label="Build"
        title="Build Project: CtrlCmd + B"
        isDisabled={this.toolbarButtonsAreDisabled()}
        onClick={() => {
          build();
        }}
      />
    );

    toolbarButtons.push(
      <Button
        key="Schema"
        icon={<GoBeakerGear />}
        label="Build Schema"
        title="Build Schema"
        isDisabled={this.toolbarButtonsAreDisabled()}
        onClick={() => {
          schema();
        }}
      />
    );

    toolbarButtons.push(
      <Button
        key="Test"
        icon={<GoCheck />}
        label="Run Tests"
        title="Run Tests"
        isDisabled={this.toolbarButtonsAreDisabled()}
        onClick={() => {
          test();
        }}
      />
    );

    if (this.props.embeddingParams.type === EmbeddingType.None) {
      toolbarButtons.push(
        <Button
          key="HelpAndPrivacy"
          icon={<GoQuestion />}
          label="Help & Privacy"
          title="Help & Privacy"
          customClassName="help"
          onClick={() => {
            this.loadHelp();
          }}
        />
      );
    }
    return toolbarButtons;
  }
  render() {
    const self = this;

    const makeEditorPanes = () => {
      const groups = this.state.tabGroups;
      const activeGroup = this.state.activeTabGroup;

      if (groups.length === 0) {
        return <div>No Groups</div>;
      }
      return groups.map((group: Group, i: number) => {
        // tslint:disable-next-line:jsx-key
        return (
          <ViewTabs
            key={`editorPane${i}`}
            views={group.views.slice(0)}
            view={group.currentView}
            preview={group.preview}
            onSplitViews={() => splitGroup()}
            hasFocus={activeGroup === group}
            onFocus={() => {
              // TODO: Should be taken care of in shouldComponentUpdate instead.
              focusTabGroup(group);
            }}
            onChangeViewType={(view, type) => setViewType(view, type)}
            onClickView={(view: View) => {
              if (!(appStore.getActiveTabGroup().currentView === view)) {
                // Avoids the propagation of content selection between tabs.
                resetDOMSelection();
              }
              focusTabGroup(group);
              openView(view);
            }}
            onDoubleClickView={(view: View) => {
              focusTabGroup(group);
              openView(view, false);
            }}
            onClose={(view: View) => {
              focusTabGroup(group);
              closeView(view);
            }}
          />
        );
      });
    };

    const editorPanes = (
      <Split
        name="Editors"
        orientation={SplitOrientation.Vertical}
        defaultSplit={{
          min: 128
        }}
        splits={this.state.editorSplits}
        onChange={(splits) => {
          this.setState({ editorSplits: splits });
          layout();
        }}
      >
        {makeEditorPanes()}
      </Split>
    );

    return (
      <div className="fill">
        <ToastContainer ref={(ref) => (this.toastContainer = ref)} />
        {this.state.newProjectDialog && (
          <NewProjectDialog
            isOpen={true}
            templatesName={this.props.embeddingParams.templatesName}
            onCancel={() => {
              this.setState({ newProjectDialog: null });
            }}
            onCreate={async (template: Template, name: string) => {
              if (!name) {
                this.showToast(<span>Project name is empty!</span>);
                return;
              }

              // create new project and save
              const newProject = new Project(name);
              await Service.loadFilesIntoProject(template.files, newProject, template.baseUrl);

              // save project before save into app store
              const fiddle = await saveProject(newProject);
              if (!fiddle) return;

              // change url
              history.replaceState({}, fiddle, `?f=${fiddle}`);

              // open new project and hide dialog
              await openProject(newProject);
              this.setState({ newProjectDialog: false });
            }}
          />
        )}
        {this.state.newFileDialogDirectory && (
          <NewFileDialog
            isOpen={true}
            directory={this.state.newFileDialogDirectory}
            onCancel={() => {
              this.setState({ newFileDialogDirectory: null });
            }}
            onCreate={async (file: File) => {
              const ret = await Service.saveFile(file);
              if (ret.success) {
                addFileTo(file, this.state.newFileDialogDirectory.getModel());
              } else {
                this.showToast(<span>${ret.message}</span>, 'error');
              }
              this.setState({ newFileDialogDirectory: null });
            }}
          />
        )}
        {this.state.editFileDialogFile && (
          <EditFileDialog
            isOpen={true}
            file={this.state.editFileDialogFile}
            onCancel={() => {
              this.setState({ editFileDialogFile: null });
            }}
            onChange={async (name: string, description) => {
              const file = this.state.editFileDialogFile.getModel();
              const ret = await Service.renameFile(file, name);
              if (ret.success) {
                updateFileNameAndDescription(file, name, description);
              } else {
                this.showToast(<span>${ret.message}</span>, 'error');
              }
              // finally close dialog
              this.setState({ editFileDialogFile: null });
            }}
          />
        )}
        {this.state.shareDialog && (
          <ShareDialog
            isOpen={true}
            fiddle={this.state.fiddle}
            onCancel={() => {
              this.setState({ shareDialog: false });
            }}
          />
        )}
        {this.state.uploadFileDialogDirectory && (
          <UploadFileDialog
            isOpen={true}
            directory={this.state.uploadFileDialogDirectory}
            onCancel={() => {
              this.setState({ uploadFileDialogDirectory: null });
            }}
            onUpload={async (files: File[]) => {
              const projectModel = this.state.project.getModel();
              const ret = await Service.saveProject(projectModel, files);
              if (ret.success) {
                files.map((file: File) => {
                  addFileTo(file, this.state.uploadFileDialogDirectory.getModel());
                });
              } else {
                this.showToast(<span>${ret.message}</span>, 'error');
              }
              this.setState({ uploadFileDialogDirectory: null });
            }}
          />
        )}
        {this.state.newDirectoryDialog && (
          <NewDirectoryDialog
            isOpen={true}
            directory={this.state.newDirectoryDialog}
            onCancel={() => {
              this.setState({ newDirectoryDialog: null });
            }}
            onCreate={(directory: Directory) => {
              // if folder empty, then no need to save it, so wait for file save
              addFileTo(directory, this.state.newDirectoryDialog.getModel());
              this.setState({ newDirectoryDialog: null });
            }}
          />
        )}
        <div style={{ height: 'calc(100% - 22px)' }}>
          <Split
            name="Workspace"
            orientation={SplitOrientation.Vertical}
            splits={this.state.workspaceSplits}
            onChange={(splits) => {
              this.setState({ workspaceSplits: splits });
              layout();
            }}
          >
            <Workspace
              project={this.state.project}
              file={this.state.file}
              onNewFile={(directory: Directory) => {
                this.setState({ newFileDialogDirectory: ModelRef.getRef(directory) });
              }}
              onEditFile={(file: File) => {
                this.setState({ editFileDialogFile: ModelRef.getRef(file) });
              }}
              onDeleteFile={async (file: File) => {
                let message = '';
                if (file instanceof Directory) {
                  message = `Are you sure you want to delete '${file.name}' and its contents?`;
                } else {
                  message = `Are you sure you want to delete '${file.name}'?`;
                }
                if (confirm(message)) {
                  const ret = await Service.deleteFile(file);
                  if (ret.success) {
                    closeTabs(file);
                    deleteFile(file);
                  } else {
                    this.showToast(<span>${ret.message}</span>, 'error');
                  }
                }
              }}
              onClickFile={(file: File) => {
                // Avoids the propagation of content selection between tabs.
                resetDOMSelection();
                openFile(file, defaultViewTypeForFileType(file.type));
              }}
              onDoubleClickFile={(file: File) => {
                if (file instanceof Directory) {
                  return;
                }
                openFile(file, defaultViewTypeForFileType(file.type), false);
              }}
              onMoveFile={(file: File, directory: Directory) => {
                addFileTo(file, directory);
              }}
              onUploadFile={(directory: Directory) => {
                this.setState({ uploadFileDialogDirectory: ModelRef.getRef(directory) });
              }}
              onNewDirectory={(directory: Directory) => {
                this.setState({ newDirectoryDialog: ModelRef.getRef(directory) });
              }}
              onDeployContract={(file: File) => {
                this.deployContract(file);
              }}
            />
            <div className="fill">
              <div style={{ height: '40px' }}>
                <Toolbar>{this.makeToolbarButtons()}</Toolbar>
              </div>
              <div style={{ height: 'calc(100% - 40px)' }}>
                <Split
                  name="Console"
                  orientation={SplitOrientation.Horizontal}
                  splits={this.state.controlCenterSplits}
                  onChange={(splits) => {
                    this.setState({ controlCenterSplits: splits });
                    layout();
                  }}
                >
                  {editorPanes}
                  <ControlCenter
                    showSandbox={this.state.showSandbox}
                    onToggle={() => {
                      const splits = this.state.controlCenterSplits;
                      splits[1].value = splits[1].value === 40 ? 256 : 40;
                      this.setState({ controlCenterSplits: splits });
                      layout();
                    }}
                  />
                </Split>
              </div>
            </div>
          </Split>
        </div>
        <StatusBar />
        <div id="task-runner-content" />
      </div>
    );
  }
}

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
import { Service, IFiddleFile } from '../service';
import ReactModal from 'react-modal';
import { Button } from './shared/Button';
import { GoGear, GoFile, GoX, Icon, GoSync, GoFileDirectory, GoCloudUpload, GoVerified, GoCode, GoFileCode } from './shared/Icons';
import { KeyboardEvent, ChangeEvent, ChangeEventHandler } from 'react';
import { ListBox, ListItem, TextInputBox } from './Widgets';
import fetchTemplates from '../utils/fetchTemplates';
import { config } from '../config';

export interface Template {
  name: string;
  description: string;
  files: IFiddleFile[];
  baseUrl: URL;
  icon: string;
}

export class NewProjectDialog extends React.Component<
  {
    isOpen: boolean;
    templatesName: string;
    onCreate: (template: Template, name: string) => void;
    onCancel: () => void;
    onOpenProject: (file: IFiddleFile) => void;
  },
  {
    description: string;
    name: string;
    template: Template;
    templates: Template[];
    recentSelect?: IFiddleFile;
  }
> {
  constructor(props: any) {
    super(props);
    this.state = {
      template: null,
      description: '',
      name: '',
      templates: [],
      recentSelect: null
    };
  }
  async componentDidMount() {
    const templatesPath = config.templates[this.props.templatesName];
    const json = await fetchTemplates(templatesPath);
    const base = new URL(templatesPath, location.href);
    const templates: Template[] = Object.entries(json).map(([key, entry]: any[]) => {
      return {
        ...entry,
        baseUrl: new URL(key + '/', base)
      };
    });

    this.setState({ templates });
    // // // default show as recents
    // // default template 0
    // if (templates.length) this.setTemplate(templates[0]);
  }

  onRecentSelectItem = (recentSelect: IFiddleFile) => {
    this.setState((s) => {
      if (s.recentSelect && s.recentSelect.name === recentSelect.name) {
        return {
          recentSelect: null
        };
      }

      return {
        recentSelect
      };
    });
  };

  onChangeName = (event: ChangeEvent<any>) => {
    this.setState({ name: event.target.value });
  };
  nameError() {
    if (this.state.name) {
      if (!/^[a-z0-9\.\-\_]+$/i.test(this.state.name)) {
        return 'Illegal characters in directory name.';
      }
    }
    return '';
  }
  async setTemplate(template: Template) {
    const description = await Service.compileMarkdownToHtml(template.description);
    this.setState({ template, description });
  }
  render() {
    return (
      <ReactModal
        onRequestClose={this.props.onCancel}
        isOpen={this.props.isOpen}
        contentLabel="Create New Project"
        className="modal show-file-icons newProject"
        overlayClassName="overlay"
        ariaHideApp={false}
      >
        <div style={{ display: 'flex', flexDirection: 'column', height: '100%' }}>
          {/* <div className="modal-title-bar">Create or open project</div> */}

          <div style={{ flex: 1, display: 'flex' }}>
            <div className="newProject-sidebar" style={{ width: 200 }}>
              <ListBox
                value={this.state.template}
                height={240}
                onSelect={(template) => {
                  if (template) {
                    this.setTemplate(template);
                  } else {
                    this.setState({ template: null });
                  }
                }}
              >
                <ListItem key="_recents" value={null} label="Recents" icon="file" />

                {this.state.templates.map((template) => {
                  return <ListItem key={template.name} value={template} label={template.name} icon={template.icon} />;
                })}
              </ListBox>
            </div>
            <div style={{ flex: 1, padding: 4, height: 310 }} className="new-project-dialog-description">
              {this.state.template ? (
                <>
                  <TextInputBox label="Name:" error={this.nameError()} value={this.state.name} onChange={this.onChangeName} />
                  <div className="desc">
                    <div className="md" dangerouslySetInnerHTML={{ __html: this.state.description }} />
                  </div>
                </>
              ) : (
                <RecentProjects onSelect={this.onRecentSelectItem} value={this.state.recentSelect} />
              )}
            </div>
          </div>

          <div className="modal-footer" style={{ justifyContent: 'flex-end' }}>
            {/* <Button
              icon={<GoX />}
              label="Cancel"
              title="Cancel"
              onClick={() => {
                this.props.onCancel();
              }}
            /> */}
            {!this.state.template && (
              <Button
                icon={<GoFileCode />}
                label="Open"
                title="Open"
                isDisabled={!this.state.recentSelect}
                onClick={() => {
                  return this.props.onOpenProject && this.props.onOpenProject(this.state.recentSelect);
                }}
              />
            )}

            {this.state.template && (
              <Button
                icon={<GoFile />}
                label="Create"
                title="Create"
                isDisabled={!this.state.template}
                onClick={() => {
                  return this.props.onCreate && this.props.onCreate(this.state.template, this.state.name);
                }}
              />
            )}

            {/* <Button
              icon={<GoFileDirectory />}
              label="More..."
              title="More..."
              onClick={() => {
                alert('Show project list of current user');
                if (this.state.name) {
                  window.location.search = `f=${this.state.name}`;
                }
              }}
            /> */}

            {/* <Button
              icon={<GoCloudUpload />}
              label="Upload"
              title="Upload"
              onClick={() => {
                alert('Upload zip folder of a project');
              }}
            /> */}
          </div>
        </div>
      </ReactModal>
    );
  }
}

type RecentProjectsState = {
  loading: boolean;
  items: IFiddleFile[];
};

class RecentProjects extends React.Component<
  {
    onSelect: (item: IFiddleFile) => void;
    value?: IFiddleFile;
  },
  RecentProjectsState
> {
  state: RecentProjectsState = {
    loading: true,
    items: []
  };

  async componentDidMount() {
    const items = await Service.getRecents();

    this.setState({
      loading: false,
      items
    });
  }

  render() {
    if (this.state.loading) {
      return <div style={{ padding: 10 }}>Loading ...</div>;
    }

    if (this.state.items.length === 0) {
      return (
        <div className="recents-empty">
          <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="60" height="60" viewBox="0 0 106.059 106.059">
            <g>
              <path
                d="M90.546,15.518C69.858-5.172,36.199-5.172,15.515,15.513C-5.173,36.198-5.171,69.858,15.517,90.547
              c20.682,20.684,54.341,20.684,75.027-0.004C111.23,69.858,111.229,36.2,90.546,15.518z M84.757,84.758
              c-17.494,17.494-45.96,17.496-63.455,0.002c-17.498-17.497-17.496-45.966,0-63.46C38.796,3.807,67.261,3.805,84.759,21.302
              C102.253,38.796,102.251,67.265,84.757,84.758z M77.017,74.001c0.658,1.521-0.042,3.286-1.562,3.943
              c-1.521,0.66-3.286-0.042-3.944-1.562c-2.893-6.689-9.73-11.012-17.421-11.012c-7.868,0-14.747,4.319-17.522,11.004
              c-0.479,1.154-1.596,1.851-2.771,1.851c-0.384,0-0.773-0.074-1.15-0.23c-1.53-0.636-2.255-2.392-1.62-3.921
              c3.71-8.932,12.764-14.703,23.063-14.703C64.174,59.371,73.174,65.113,77.017,74.001z M33.24,38.671
              c0-3.424,2.777-6.201,6.201-6.201c3.423,0,6.2,2.776,6.2,6.201c0,3.426-2.777,6.202-6.2,6.202
              C36.017,44.873,33.24,42.097,33.24,38.671z M61.357,38.671c0-3.424,2.779-6.201,6.203-6.201c3.423,0,6.2,2.776,6.2,6.201
              c0,3.426-2.776,6.202-6.2,6.202S61.357,42.097,61.357,38.671z"
              />
            </g>
          </svg>
          <br />
          You don't have any project yet.
        </div>
      );
    }

    return (
      <div
        style={{
          padding: 10,
          boxSizing: 'border-box',
          overflow: 'auto'
        }}
        className={this.props.value ? 'has-selected' : ''}
      >
        {this.state.items.map((s) => (
          <div key={s.name} className={`recent-item ${this.props.value && s.name === this.props.value.name ? 'selected' : ''}`} onClick={() => this.props.onSelect(s)}>
            <GoFileDirectory /> {s.name}
          </div>
        ))}
      </div>
    );
  }
}

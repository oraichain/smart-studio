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
import { GoGear, GoFile, GoX, Icon, GoSync, GoFileDirectory, GoCloudUpload, GoVerified, GoCode } from './shared/Icons';
import { KeyboardEvent, ChangeEvent, ChangeEventHandler } from 'react';
import { ListBox, ListItem, TextInputBox } from './Widgets';
import fetchTemplates from '../utils/fetchTemplates';
import getConfig from '../config';

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
  },
  {
    description: string;
    name: string;
    template: Template;
    templates: Template[];
  }
> {
  constructor(props: any) {
    super(props);
    this.state = {
      template: null,
      description: '',
      name: '',
      templates: []
    };
  }
  async componentDidMount() {
    const config = await getConfig();
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
    // default template 0
    if (templates.length) this.setTemplate(templates[0]);
  }
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
      <ReactModal isOpen={this.props.isOpen} contentLabel="Create New Project" className="modal show-file-icons newProject" overlayClassName="overlay" ariaHideApp={false}>
        <div style={{ display: 'flex', flexDirection: 'column', height: '100%' }}>
          {/* <div className="modal-title-bar">Create or open project</div> */}

          <div style={{ display: 'flex', flex: 1 }}>
            <div style={{ width: 200 }}>
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
            <div style={{ flex: 1, padding: 4, height: 350 }} className="new-project-dialog-description">
              {this.state.template ? (
                <>
                  <TextInputBox label="Name:" error={this.nameError()} value={this.state.name} onChange={this.onChangeName} />
                  <div className="desc">
                    <div className="md" dangerouslySetInnerHTML={{ __html: this.state.description }} />
                  </div>
                </>
              ) : (
                <RecentProjects />
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
            <Button
              icon={<GoFile />}
              label="Create"
              title="Create"
              isDisabled={!this.state.template}
              onClick={() => {
                return this.props.onCreate && this.props.onCreate(this.state.template, this.state.name);
              }}
            />
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

class RecentProjects extends React.Component {
  render() {
    return <div>dffdfgdfg</div>;
  }
}

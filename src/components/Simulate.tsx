import React from 'react';
import ReactCodeMirror from '@uiw/react-codemirror';
import { json } from '@codemirror/lang-json';
import { fromAscii, fromBase64 } from '@cosmjs/encoding';
import beautify from 'json-beautify-fix';
import { VMInstance, BasicBackendApi, BasicKVIterStorage, BasicQuerier, IBackend } from '../cwvm';
import { Env, MessageInfo } from '../cwvm/types';
import appStore from '../stores/AppStore';

const backend: IBackend = {
  backend_api: new BasicBackendApi('orai'),
  storage: new BasicKVIterStorage(),
  querier: new BasicQuerier()
};

const vm = new VMInstance(backend);

const mockEnv: Env = {
  block: {
    height: 1337,
    time: '2000000000',
    chain_id: 'Oraichain'
  },
  contract: {
    address: 'orai1qxd52frq6jnd73nsw49jzp4xccal3g9v47pxwftzqy78ww02p75s62e94t'
  }
};

const mockInfo: MessageInfo = {
  sender: 'orai1602dkqjvh4s7ryajnz2uwhr8vetrwr8nekpxv5',
  funds: []
};

export interface SimulateProps {
  projectName: string;
  placeholder: string;
}

export interface SimulateState {
  payload: string;
  output: any;
  isValid: boolean;
}

export class Simulate extends React.Component<SimulateProps, SimulateState> {
  constructor(props: SimulateProps) {
    super(props);
    this.state = {
      payload: '',
      output: {},
      isValid: true
    };
  }

  async componentDidMount() {
    const wasmFilePath = `artifacts/${this.props.projectName}.wasm`;
    const project = appStore.getProject().getModel();
    const wasmFile = project.getFile(wasmFilePath);
    if (wasmFile) {
      await vm.build(wasmFile.data as ArrayBuffer);
    } else {
      this.setIsValid(false);
    }
  }

  instantiate = () => {
    try {
      const instantiateRes = vm.instantiate(mockEnv, mockInfo, JSON.parse(this.state.payload));
      this.setOutput((instantiateRes.json as { ok: any }).ok);
    } catch {}
  };

  execute = () => {
    try {
      const instantiateRes = vm.execute(mockEnv, mockInfo, JSON.parse(this.state.payload));
      this.setOutput((instantiateRes.json as { ok: any }).ok);
    } catch {}
  };

  query = () => {
    try {
      const instantiateRes = vm.query(mockEnv, JSON.parse(this.state.payload));
      const data = (instantiateRes.json as { ok: any }).ok;
      this.setOutput(JSON.parse(fromAscii(fromBase64(data))));
    } catch {}
  };

  setIsValid = (isValid: boolean) => {
    this.setState({ isValid });
  };

  setPayload = (payload: string) => {
    this.setState({ payload });
  };

  setOutput = (output: string) => {
    this.setState({ output });
  };

  render() {
    const { payload, output, isValid } = this.state;
    if (!isValid) return <div>Wasm file not found!</div>;
    return (
      <div className="fill">
        <table className="simulate">
          <thead>
            <tr>
              <th>Input</th>
              <th>Actions</th>
              <th>Output</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td width={'45%'}>
                <ReactCodeMirror
                  minHeight="120px"
                  className="output"
                  value={payload}
                  extensions={[json()]}
                  editable
                  theme="dark"
                  onChange={this.setPayload}
                  placeholder={this.props.placeholder}
                  style={{ border: 'none', height: '100%' }}
                />
              </td>
              <td width={'10%'}>
                <div className="actions">
                  <div className="button" title="Instantiate" onClick={this.instantiate}>
                    instantiate
                  </div>
                  <div className="button" title="Execute" onClick={this.execute}>
                    execute
                  </div>
                  <div className="button" title="Query" onClick={this.query}>
                    query
                  </div>
                </div>
              </td>
              <td width={'45%'}>
                <ReactCodeMirror minHeight="120px" className="output" theme="dark" value={beautify(output, null, 2)} extensions={[json()]} readOnly />
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    );
  }
}

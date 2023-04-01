import React from 'react';
import ReactCodeMirror from '@uiw/react-codemirror';
import { json } from '@codemirror/lang-json';
import { fromAscii, fromBase64 } from '@cosmjs/encoding';
import appStore from '../stores/AppStore';
import { Env, MessageInfo, VMInstance, BasicBackendApi, BasicKVIterStorage, BasicQuerier, IBackend } from '@terran-one/cosmwasm-vm-js';
import { default as init, Poseidon, curve_hash, groth16_verify, keccak_256, sha256 } from 'cosmwasm-vm-js-zk/web';

// update zk wasm implementation
init().then(() => {
  const poseidon = new Poseidon();
  VMInstance.poseidon_hash = poseidon.hash.bind(poseidon);
  VMInstance.curve_hash = curve_hash;
  VMInstance.groth16_verify = groth16_verify;
  VMInstance.keccak_256 = keccak_256;
  VMInstance.sha256 = sha256;
});

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
  info: string;
  output: any;
  isValid: boolean;
  actionWidth: number;
  height: number;
  width: number;
}

export class Simulate extends React.Component<SimulateProps, SimulateState> {
  container: HTMLDivElement;
  constructor(props: SimulateProps) {
    super(props);
    this.state = {
      payload: '',
      actionWidth: 100,
      height: 0,
      width: 0,
      info: JSON.stringify(mockInfo, null, 2),
      output: null,
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
    document.addEventListener('layout', this.onLayout);
    this.onLayout();
  }

  componentWillUnmount() {
    document.removeEventListener('layout', this.onLayout);
  }

  onLayout = () => {
    if (this.container) {
      this.setState({ height: this.container.offsetHeight - 100, width: this.container.offsetWidth - this.state.actionWidth - 60 });
    }
  };

  instantiate = () => {
    try {
      const res = vm.instantiate(mockEnv, JSON.parse(this.state.info), JSON.parse(this.state.payload));
      const error = (res.json as { error: string }).error;
      if (error) throw new Error(error);
      this.setOutput((res.json as { ok: any }).ok);
    } catch (ex) {
      alert(ex.message);
    }
  };

  execute = () => {
    try {
      const res = vm.execute(mockEnv, JSON.parse(this.state.info), JSON.parse(this.state.payload));
      const error = (res.json as { error: string }).error;
      if (error) throw new Error(error);
      this.setOutput((res.json as { ok: any }).ok);
    } catch (ex) {
      alert(ex.message);
    }
  };

  query = () => {
    try {
      const res = vm.query(mockEnv, JSON.parse(this.state.payload));
      const error = (res.json as { error: string }).error;
      if (error) throw new Error(error);
      const data = (res.json as { ok: any }).ok;
      this.setOutput(JSON.parse(fromAscii(fromBase64(data))));
    } catch (ex) {
      alert(ex.message);
    }
  };

  setIsValid = (isValid: boolean) => {
    this.setState({ isValid });
  };

  setInfo = (info: string) => {
    this.setState({ info });
  };

  setPayload = (payload: string) => {
    this.setState({ payload });
  };

  setOutput = (output: string) => {
    this.setState({ output });
  };

  setContainer(div: HTMLDivElement) {
    this.container = div;
  }

  render() {
    const { payload, output, info, isValid, height, width, actionWidth } = this.state;
    const editorWidth = width ? Math.round(width / 2) + 'px' : '';
    const editorHeight = height ? height + 'px' : '';
    const editorHeightInfo = 80;
    const editorHeightInput = height ? height - editorHeightInfo + 'px' : '';
    if (!isValid) return <div>Wasm file not found!</div>;
    return (
      <div className="fill" ref={(ref) => this.setContainer(ref)}>
        <table className="simulate">
          <thead>
            <tr>
              <th>Message Info & Input</th>
              <th>Actions</th>
              <th>Output</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td>
                <ReactCodeMirror
                  basicSetup={{ lineNumbers: false, foldGutter: false, searchKeymap: false }}
                  className="input"
                  width={editorWidth}
                  height={editorHeightInfo + 'px'}
                  value={info}
                  extensions={[json()]}
                  editable
                  theme="dark"
                  onChange={this.setInfo}
                />

                <ReactCodeMirror
                  basicSetup={{ lineNumbers: false, foldGutter: false, searchKeymap: false }}
                  maxHeight={editorHeightInput}
                  width={editorWidth}
                  className="input"
                  value={payload}
                  extensions={[json()]}
                  editable
                  theme="dark"
                  onChange={this.setPayload}
                  placeholder={this.props.placeholder}
                />
              </td>
              <td>
                <div className="actions" style={{ width: actionWidth }}>
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
              <td>
                <ReactCodeMirror
                  width={editorWidth}
                  maxHeight={editorHeight}
                  className="output"
                  theme="dark"
                  value={JSON.stringify(output, null, 2)}
                  extensions={[json()]}
                  readOnly
                />
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    );
  }
}

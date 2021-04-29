import React from 'react';
import { Terminal } from 'xterm';
import { AttachAddon } from 'xterm-addon-attach';
import {FitAddon} from 'xterm-addon-fit'
import { Service } from '../service';
import { getServiceURL, ServiceTypes } from '../compilerServices/sendRequest';

export interface SimulateProps {
  projectName: string
}

export class Simulate extends React.Component<SimulateProps, {}> {
  container: HTMLDivElement;
  xterm: Terminal;
  socket: WebSocket;
  timer: number;
  pid: string;

  private setContainer(container: HTMLDivElement) {
    if (container == null) {
      return;
    }
    this.container = container;
  }

  componentWillUnmount() {
    this.xterm?.dispose();
    this.socket?.close();
    window.clearInterval(this.timer);
  }

  isForceClose = false;
  async componentDidMount() {
    // Attach the socket to term    
    this.xterm = new Terminal();
    const fitAddon = new FitAddon();
    this.xterm.loadAddon(fitAddon);
    // auto fit
    this.timer = window.setInterval(()=>{
        fitAddon.fit();
    }, 1000);    
    this.xterm.open(this.container);

    this.xterm.onResize((size: { cols: number, rows: number }) => {
        if (!this.pid) {
            return;
        }
        const cols = size.cols;
        const rows = size.rows;
        Service.resizeTerminal(this.pid, cols, rows);        
    });

    const pid = await Service.createTerminal(this.props.projectName, this.xterm.cols,this.xterm.rows);
    if(pid){          
      this.socket = await Service.createTerminalSocket(pid);
      this.socket.onclose = () => {
        if (! this.isForceClose) {
          this.xterm.write('\n\n\x1b[31mSimulate session is terminated due to no actions, please re-active!');
        }
      }
      const attachAddon = new AttachAddon(this.socket);     
      this.pid = pid;   

      let baseURL = await getServiceURL(ServiceTypes.Service);      
      if (!baseURL.startsWith('http')){
        baseURL = window.location.protocol + baseURL;
      }
      this.xterm.write(`🚀  Rest Server is start at \x1b[32m\x1b[1m${baseURL}/terminals/${pid}/wasm\x1b[0m\n\n\r`);
      this.xterm.loadAddon(attachAddon);                
    } else {
      this.xterm.write(`\x1b[31mWasm file is not built`);
    }


  }

  shouldComponentUpdate(nextProps: any, nextState: any) {
    if (this.props.projectName === nextProps.projectName) {
      return false;
    }

    return true;
  }

  async componentDidUpdate() {
    // reload terminal
    this.isForceClose = true;
    await this.componentWillUnmount();
    await this.componentDidMount();
    this.isForceClose = false;
  }

  render() {
    return <div className="fill" ref={(ref) => this.setContainer(ref)} />;
  }
}

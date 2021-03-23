import React from 'react';
import { Terminal } from 'xterm';
import { AttachAddon } from 'xterm-addon-attach';
import {FitAddon} from 'xterm-addon-fit'
import { Service } from '../service';
import '../../style/xterm.css';

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

    const processId = await Service.createTerminal(this.props.projectName, this.xterm.cols,this.xterm.rows);
    if(processId){          
      this.socket = await Service.createTerminalSocket(processId);
      const attachAddon = new AttachAddon(this.socket);     
      this.pid = processId;   
      this.xterm.loadAddon(attachAddon);                
    } else {
      this.xterm.write(`Wasm file is not built`);
    }      
    
  }

  render() {
    return <div className="fill" ref={(ref) => this.setContainer(ref)} />;
  }
}

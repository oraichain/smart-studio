import getConfig from '../config';
import React from 'react';
import ReactModal from 'react-modal';
import { Button } from './shared/Button';
import { Github } from './shared/Icons';
import { Loading } from './Loading';
import { sendRequest } from '@/compilerServices/sendRequest';

type GithubConnectDialogState = {
  isOpen: boolean,
  onClose: () => void
}

export class GithubConnectDialog extends React.PureComponent<GithubConnectDialogState> {
  state = {
    loading: false
  }

  async handleConnectGithub() {
    const config = await getConfig();
    window.open(config.serviceUrl + '/auth/github', '_self');
  }

  componentDidMount() {
    const urlParams = new URLSearchParams(window.location.search);
    const code = urlParams.get('code');

    const { onClose } = this.props;

    // auto close
    const user = localStorage.getItem('__USER__');
    if (user) {
      onClose();
      return;
    }

    if (code) {
      this.setState({ loading: true });

      (async () => {
        const config = await getConfig();

        const res = await fetch(config.serviceUrl + '/auth/github?code=' + code);

        if (res.status === 200) {
          const data = await res.json();

          localStorage.setItem('__USER__', JSON.stringify(data));
          history.pushState({}, document.title, location.pathname);
          onClose();
        } else {
          location.pathname = '/';
        }
      })();

    }
  }

  render() {
    const { isOpen } = this.props;
    const { loading } = this.state;

    return (
      <ReactModal 
        isOpen={isOpen} 
        contentLabel="Connect to github"
        className="modal show-file-icons connectGithub"
        overlayClassName="overlay" ariaHideApp={false}>
          <div style={{ position: 'relative', display: 'flex', flexDirection: 'column', height: '100%' }}>
            <div className="modal-title-bar">Connect to github</div>

            {loading && <Loading /> }
            <div className="modal-content" style={{ background: '#fff', textAlign: 'center' }}>
              <img src="https://orai.io/images/OraiBanner.svg" height="320" style={{ margin: 20 }} />
            </div>
  
            <div className="modal-footer">
              <Button
                fill
                icon={<Github />}
                label="Connect"
                title="Connect"
                onClick={this.handleConnectGithub}
              />
            </div>
          </div>
        </ReactModal>
    )
  }
}
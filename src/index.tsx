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
import ReactDOM from 'react-dom';

import { App, EmbeddingParams, EmbeddingType } from './components/App';
import { layout } from './util';
import { MonacoUtils } from './monaco-utils';

import '../assets/global.css';

export function forEachUrlParameter(callback: (key: string, value: any) => void) {
  let url = window.location.search.substring(1);
  url = url.replace(/\/$/, ''); // Replace / at the end that gets inserted by browsers.
  url.split('&').forEach(function(s: any) {
    const t = s.split('=');
    if (t.length === 2) {
      callback(t[0], decodeURIComponent(t[1]));
    } else {
      callback(t[0], true);
    }
  });
}

export function getUrlParameters(): any {
  const params: any = {};
  forEachUrlParameter((key, value) => {
    params[key] = value;
  });
  return params;
}

export const appWindowContext = {
  promptWhenClosing: false
};

export function unloadPageHandler(e: { returnValue: string }): any {
  if (!appWindowContext.promptWhenClosing) {
    window.removeEventListener('beforeunload', unloadPageHandler, false);
    return;
  }
  e.returnValue = 'Project data is not saved.';
}

export function getEmbeddingParams(parameters: any): EmbeddingParams {
  const embedding = parameters['embedding'];
  let type;
  switch (embedding) {
    case 'default':
      type = EmbeddingType.Default;
      break;
    case 'arc_website':
      type = EmbeddingType.Arc;
      break;
    default:
      const embed = parameters['embed'] === true ? true : !!parseInt(parameters['embed'], 10);
      type = embed ? EmbeddingType.Default : EmbeddingType.None;
      break;
  }
  const templatesName = parameters['embedding'] === 'arc_website' ? 'arc' : 'default';
  return {
    type,
    templatesName
  };
}

const keystationUrl = process.env.WALLET_URL;
const loadKeyStation = (callback: any) => {
  const script = document.createElement('script');
  script.src = `${keystationUrl}/lib/keystation.js`;
  script.async = true;
  script.onload = () => {
    const keystation = new window.Keystation({
      keystationUrl,
      lcd: process.env.LCD
    });
    callback(keystation);
  };
  script.onerror = (error) => {
    // handle error
    console.log('Can not load keystation library!');
    callback(null);
  };

  document.body.appendChild(script);
};

export async function init(environment = 'production') {
  // Logger.init();
  window.addEventListener('resize', layout, false);
  window.addEventListener('beforeunload', unloadPageHandler, false);
  const parameters = getUrlParameters();
  const update = parameters['update'] === true ? true : !!parseInt(parameters['update'], 10);
  const fiddle = parameters['fiddle'] || parameters['f'];
  const embeddingParams = getEmbeddingParams(parameters);
  try {
    MonacoUtils.initialize();
    loadKeyStation((keystation: any) => {
      ReactDOM.render(
        <App keystation={keystation} update={update} fiddle={fiddle} embeddingParams={embeddingParams} windowContext={appWindowContext} />,
        document.getElementById('app')
      );
    });
  } catch (e) {
    console.error(e);
  }
}

init();

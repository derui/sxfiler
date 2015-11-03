import App from 'app';
import BrowserWindow from 'browser-window';
import {IPCKeys} from 'sxfiler/common/Constants';

import Path from 'path';
import CrashReporter from 'crash-reporter';
import MainIPC from 'sxfiler/main/MainIPC';

CrashReporter.start();

/**
 * This is Main process.
 */
class Main {
  /**
   * Constructor of Main
   */
  constructor() {
    this.mainWindow = null;
  }

  /**
   * Handler for Application ready event (App.on('ready', ...))
   */
  onReady() {
    this.mainWindow = new BrowserWindow({
      width: 800,
      height: 600,
      resizable: true
    });

    const filePath = Path.join(__dirname, 'index.html');
    this.mainWindow.loadUrl('file://' + filePath);
  }

  /**
   * A Handler for quiting application
   */
  onQuit() {
    if (!this.mainWindow) {
      return;
    }

    this.mainWindow.close();
    this.mainWindow = null;
  }
}

const ipc = require('ipc');
const mainIpc = new MainIPC(ipc, require('original-fs'));
mainIpc; // ignore lint error.


/**
 * A Singleton object of Main
 */
const main = new Main();
App.on('ready', () =>{
  main.onReady();
});
App.on('window-all-closed', () => {
  App.quit();
});

ipc.on(IPCKeys.QUIT_APPLICATION, () => {
  main.onQuit();
});

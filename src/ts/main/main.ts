import App from 'app';
import BrowserWindow from 'browser-window';
import {IPC_KEYS} from '../common/constants';

import * as Path from 'path';
import CrashReporter from 'crash-reporter';
import MainIPC from './main-ipc';

CrashReporter.start();

/**
 * This is Main process.
 */
export default class Main {
  private mainWindow: any = null;

  /**
   * Handler for Application ready event (App.on('ready', ...))
   */
  public onReady(): void {
    this.mainWindow = new BrowserWindow({
      height: 600,
      resizable: true,
      width: 800,
      acceptFirstMouse: true
    });

    const filePath = Path.join(__dirname, 'index.html');
    this.mainWindow.loadURL('file://' + filePath);
    this.mainWindow.focusOnWebView();
  }

  /**
   * A Handler for quiting application
   */
  public onQuit(): void {
    if (!this.mainWindow) {
      return;
    }

    this.mainWindow.close();
    this.mainWindow = null;
  }
}

const ipc = require( 'electron' ).ipcMain;
const mainIpc = new MainIPC(ipc, require( 'original-fs' ));

/**
 * A Singleton object of Main
 */
const main = new Main();
App.on('ready', () => {
  main.onReady();
});

App.on('window-all-closed', () => {
  App.quit();
});

ipc.on(IPC_KEYS.QUIT_APPLICATION, () => {
  main.onQuit();
});

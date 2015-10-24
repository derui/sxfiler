import App from 'app';
import BrowserWindow from 'browser-window';

import Path from 'path';
import CrashReporter from 'crach-reporter';
CrashReporter.start();

class Main {
  constructor() {
    this.mainWindow = null;
  }

  onReady() {
    this.mainWindow = new BrowserWindow({
      width: 800,
      height: 600,
      resizable: true
    });

    const filePath = Path.join(__dirname, 'index.html');
    this.mainWindow.loadUrl('file://' + filePath);
  }
}

const main = new Main();
App.on('ready', main.onReady);

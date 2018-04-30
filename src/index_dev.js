const electron = require('electron');
const crashReporter = electron.crashReporter;
const path = require('path');

if (require.main !== module) {
  process.exit(1);
}

const app = electron.app;

const browserWindow = new electron.BrowserWindow({
  height: 600,
  width: 800,
  resizable: true,
  acceptFirstMouse: true,
  webPreferences: undefined,
});

if (!browserWindow) {
  console.error("Can not open window");
  process.exit(1);
}

app.on('ready', () => {
  browserWindow.loadURL(`file://${path.join(__dirname, "index.html")}`);
  browserWindow.focusOnWebView();
});

app.on('window-all-closed', () => app.quit());

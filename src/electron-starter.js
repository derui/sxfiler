const electron = require("electron");
const path = require("path");
const url = require("url");

if (require.main !== module) {
  process.exit(1);
}

const app = electron.app;

app.on('ready', () => {
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

  const startUrl = process.env.ELECTRON_START_URL || url.format({
    pathname: path.join(__dirname, '/../build/index.html'),
    protocol: 'file:',
    slashes: true
  });
  browserWindow.loadURL(startUrl);
  browserWindow.focusOnWebView();
});

app.on('window-all-closed', () => app.quit());

electron.ipcMain.on('quit', () => app.quit());

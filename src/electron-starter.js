const electron = require("electron");
const path = require("path");
const url = require("url");
const shell = electron.shell

if (require.main !== module) {
  process.exit(1);
}

const app = electron.app;
// it will be 'true' in Electron 9.
app.allowRendererProcessReuse = true;

app.on('ready', () => {
  const browserWindow = new electron.BrowserWindow({
    height: 600,
    width: 800,
    resizable: true,
    acceptFirstMouse: true,
    webPreferences: {
      preload: path.join(app.getAppPath(), 'public', 'preload.js'),
    },
  });

  if (!browserWindow) {
    console.error("Can not open window");
    process.exit(1);
  }

  const startUrl = process.env.ELECTRON_START_URL || url.format({
    pathname: path.join(__dirname, '/../public/index.html'),
    protocol: 'file:',
    slashes: true
  });
  browserWindow.loadURL(startUrl);
  browserWindow.focusOnWebView();
});

app.on('window-all-closed', () => app.quit());

electron.ipcMain.on('quit', () => app.quit());
electron.ipcMain.on('open-item', (e, itemPath) => {
  shell.openPath(itemPath);
})

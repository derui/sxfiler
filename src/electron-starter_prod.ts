import * as electron from "electron";
import * as path from "path";
import {spawn} from "child_process";

if (require.main !== module) {
  process.exit(1);
}

const app = electron.app;
const server = spawn(path.join(__dirname, "sxfiler-server")).on("error", (error: undefined | Error) => {
  if (error) {
    throw error;
  }
});

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

  browserWindow.loadURL(`file://${path.join(__dirname, "index.html")}`);
  browserWindow.focusOnWebView();
});

app.on('window-all-closed', () => {
  server.kill();
  app.quit();
});

electron.ipcMain.on('quit', () => {
  server.kill();
  app.quit();
});

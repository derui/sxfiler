const fs = require('fs');
const electron = require('electron');
const path = require('path');
const spawn = require('child_process').spawn;
const os = require('os');

if (require.main !== module) {
  process.exit(1);
}

// get the directory to save user configuration
function getConfigDir() {
  const basePath = process.env.XDG_CONFIG_HOME || path.join(os.homedir(), '.config');
  const configDir = path.join(basePath, 'sxfiler');

  return configDir;
}

// spawn a server
function spawnServer(configDir) {
  console.log('Launching server...');

  const configPath = path.join(configDir, 'config.json');
  if (!fs.existsSync(configPath)) {
    fs.copyFileSync(path.join(__dirname, 'defaults', 'config.json'), configPath);
  }
  const keyMapPath = path.join(configDir, 'keymap.json');
  if (!fs.existsSync(keyMapPath)) {
    fs.copyFileSync(path.join(__dirname, 'defaults', 'keymap.json'), keyMapPath);
  }

  const server = spawn(
    path.join(`${__dirname}.unpacked`, 'sxfiler_server'),
    [`--config=${configPath}`, '-d', path.join(configDir, 'dict')],
    {
      stdio: 'inherit',
    }
  ).on('error', error => {
    if (error) {
      throw error;
    }
  });
  return server;
}

const configDir = getConfigDir();
if (!fs.existsSync(configDir)) {
  fs.mkdirSync(configDir, { recursive: true });
}

const app = electron.app;
const server = spawnServer(configDir);

app.on('ready', () => {
  const browserWindow = new electron.BrowserWindow({
    height: 600,
    width: 800,
    resizable: true,
    acceptFirstMouse: true,
    webPreferences: undefined,
  });

  if (!browserWindow) {
    console.error('Can not open window');
    process.exit(1);
  }

  browserWindow.loadURL(`file://${path.join(__dirname, 'index.html')}`);
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

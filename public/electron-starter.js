const fs = require('fs');
const electron = require('electron');
const path = require('path');
const spawn = require('child_process').spawn;
const os = require('os');
const net = require('net');

if (require.main !== module) {
  process.exit(1);
}

function checkAvailablePort(port) {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.unref();
    server.on('error', reject);
    server.listen({ port }, () => {
      const { port } = server.address();
      server.close(() => {
        resolve(port);
      });
    });
  });
}

// find available port
async function findAvailablePort() {
  const portRange = { min: 1024, max: 65535 };

  for (let i = portRange.min; i <= portRange.max; i++) {
    try {
      return await checkAvailablePort(i);
    } catch (error) {
      if (error.code !== 'EADDRINUSE') {
        throw error;
      }
    }
  }
}

// get the directory to save user configuration
function getConfigDir() {
  const basePath = process.env.XDG_CONFIG_HOME || path.join(os.homedir(), '.config');
  const configDir = path.join(basePath, 'sxfiler');

  return configDir;
}

function setupConfigurations(configDir) {
  const configPath = path.join(configDir, 'config.json');
  if (!fs.existsSync(configPath)) {
    fs.copyFileSync(path.join(__dirname, 'defaults', 'config.json'), configPath);
  }

  const keyMapPath = path.join(configDir, 'keymap.json');
  if (!fs.existsSync(keyMapPath)) {
    fs.copyFileSync(path.join(__dirname, 'defaults', 'keymap.json'), keyMapPath);
  }

  const dictDir = path.join(configDir, 'dict');
  if (!fs.existsSync(dictDir)) {
    fs.mkdirSync(dictDir);
  }
}

// spawn a server
function spawnServer(configDir, port) {
  console.log('Launching server...');

  const server = spawn(
    path.join(`${__dirname}.unpacked`, 'sxfiler_server.exe'),
    ['--config', configDir, '-d', path.join(configDir, 'dict'), '--port', port],
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
setupConfigurations(configDir);

async function launch() {
  const port = await findAvailablePort();
  const app = electron.app;

  const server = spawnServer(configDir, port);
  process.env.SERVER_URL = `ws://localhost:${port}`;

  app.on('ready', () => {
    const browserWindow = new electron.BrowserWindow({
      height: 600,
      width: 800,
      resizable: true,
      acceptFirstMouse: true,
      webPreferences: {
        preload: path.join(app.getAppPath(), 'preload.js'),
      },
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

  app.on('quit', () => {
    server.kill();
    app.quit();
  });

  electron.ipcMain.on('quit', () => {
    server.kill();
    app.quit();
  });
}

launch();

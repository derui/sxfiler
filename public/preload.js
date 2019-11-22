'use strict';
const { ipcRenderer } = require('electron');

window.ipcRenderer = ipcRenderer;
window.applicationConfig = {
  serverURL: process.env.SERVER_URL,
};

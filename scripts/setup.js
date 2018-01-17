const fs = require('fs');

if (!fs.existsSync('./dist')) {
  fs.mkdirSync('./dist');
  fs.mkdirSync('./dist/web');
}

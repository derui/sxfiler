var path = require('path');
module.exports = {
  aliases: {},
  replacements: {
    '^sxfiler/(.+$)': function(alias, regexMatch, regexObject) {
      var match = alias.match(regexObject);
      if (!match) {
        return alias;
      }

      return path.resolve(path.join('./src/ts', match[1]));
    }
  }
};

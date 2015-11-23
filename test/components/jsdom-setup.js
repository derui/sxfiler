import * as jsdom from 'jsdom';

export function setup(t) {
  t('setup', (t) => {
    global.document = jsdom.jsdom('<!doctype html><html><body></body></html>');
    global.window = document.defaultView;
    global.Element = global.window.Element;
    global.navigator = {
      userAgent: 'node.js'
    };

    t.end();
  });
}

export function cleanup(t) {
  t('setup', (t) => {
    global.document = null;
    global.window = null;
    global.Element = null;
    global.navigator = {
      userAgent: 'node.js'
    };

    t.end();
  });
}

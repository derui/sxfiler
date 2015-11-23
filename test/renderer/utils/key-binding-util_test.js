import test from 'tape';
import R from 'ramda';
import {getKeyIdentifier, convertEventToDescriptor} from 'sxfiler/renderer/utils/key-binding-util';

const wrapper = (label, f) => test(`renderer/utils/key-binding-util ${label}`, f);

wrapper('getKeyIdentifier return null when give null/undefined', (t) => {
  t.equal(getKeyIdentifier(null), null);
  t.equal(getKeyIdentifier(undefined), null);
  t.end();
});

wrapper('getKeyIdentifier return combination identifier each keys', (t) => {
  t.equal(getKeyIdentifier({key: 'j'}), 'j');
  t.equal(getKeyIdentifier({key: 'j', ctrlKey: true}), 'C-j');
  t.equal(getKeyIdentifier({key: 'j', shiftKey: true}), 'S-j');
  t.equal(getKeyIdentifier({key: 'j', altKey: true}), 'A-j');
  t.equal(getKeyIdentifier({key: 'j', metaKey: true}), 'M-j');

  t.equal(getKeyIdentifier({
    key: 'j',
    ctrlKey: true,
    shiftKey: true,
    altKey: true,
    metaKey: true
  }), 'C-S-A-M-j');
  t.end();
});

wrapper('convertEventToDescriptor should return the descriptor converted from event', (t) => {
  t.deepEqual(convertEventToDescriptor({which: 'j'}), {
    key: 'j',
    ctrlKey: false,
    shiftKey: false,
    altKey: false,
    metaKey: false
  });

  t.deepEqual(convertEventToDescriptor({which: 'j', metaKey: true}), {
    key: 'j',
    ctrlKey: false,
    shiftKey: false,
    altKey: false,
    metaKey: true
  });
  t.end();
});

wrapper('convertEventToDescriptor should return null when given invalid event', (t) => {
  t.deepEqual(convertEventToDescriptor({ch: 'j'}), {
    key: undefined,
    ctrlKey: false,
    altKey: false,
    metaKey: false,
    shiftKey: false
  });
  t.equal(convertEventToDescriptor(undefined), null);
  t.equal(convertEventToDescriptor(null), null);
  t.equal(convertEventToDescriptor(1), null);
  t.equal(convertEventToDescriptor('1'), null);
  t.end();
});

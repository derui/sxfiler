import sinon from 'sinon';
import EventEmitter from 'events';

/**
 * Make stubbed ipc object.
 *
 * Resulting this has very simple stubbing imiplemented methods.
 */
export default function createIpc() {
  const ipc = new EventEmitter();
  ipc.send = function(name, ...args) {
    ipc.emit(name, {
      sender: {
        send(name, ...args) {
          ipc.emit(name, ...args);
        }
      }
    }, ...args);
  };
  return ipc;
};

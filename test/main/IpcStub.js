import sinon from 'sinon';

/**
 * Make stubbed ipc object.
 *
 * Resulting this has very simple stubbing imiplemented methods.
 */
export default function make() {
  return {
    _handlers: {},
    on(channel, handler) {
      if (!this._handlers[channel]) {
        this._handlers[channel] = [];
      }
      this._handlers[channel].push(handler);
    },
    send(channel, ...args) {
      if (!this._handlers[channel]) {
        return;
      }
      let handlers = this._handlers;
      let ev = {
        sender: {
          send(channel, ...args) {
            handlers[channel].forEach((handler) => handler(args));
          }
        }
      };
      this._handlers[channel].forEach((handler) => handler(ev, args));
    }
  };
}

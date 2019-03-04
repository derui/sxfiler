import * as types from "./types/index";

export interface DispatcherLike<P> extends types.Dispatcher<P>, types.Observer<P> {}

export class Dispatcher<P> implements DispatcherLike<P> {
  private handlers: Array<types.Subscriber<P>>;

  constructor() {
    this.handlers = [];
  }

  public dispatch(payload: P) {
    this.handlers.forEach(f => f(payload));
  }

  public subscribe(subscriber: types.Subscriber<P>) {
    if (this.handlers.find(v => v === subscriber)) {
      return;
    }
    this.handlers.push(subscriber);
  }

  public unsubscribe(subscriber: types.Subscriber<P>) {
    this.handlers = this.handlers.filter(v => v !== subscriber);
  }
}

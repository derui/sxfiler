import * as types from "@/types";

export interface DispatcherLike<P> extends types.Dispatcher<P>, types.Observer<P> {}

export class Dispatcher<P> implements DispatcherLike<P> {
  private handlers: types.Subscriber<P>[];

  public constructor() {
    this.handlers = [];
  }

  public dispatch(payload: P): void {
    this.handlers.forEach((f) => f(payload));
  }

  public subscribe(subscriber: types.Subscriber<P>): void {
    if (this.handlers.find((v) => v === subscriber)) {
      return;
    }
    this.handlers.push(subscriber);
  }

  public unsubscribe(subscriber: types.Subscriber<P>): void {
    this.handlers = this.handlers.filter((v) => v !== subscriber);
  }
}

// handler definition
export type Handler = {
  handle(ev: MessageEvent): void;
};

// This module defines handler of WebSocket.
export class WebSocketHandler {
  private handlers: Handler[];
  constructor(private ws: WebSocket) {
    this.handlers = [];
  }

  /**
   *  initialize message handler for WebSocket.
   */
  public initialize() {
    this.ws.onmessage = ev => this.handlers.forEach(handler => handler.handle(ev));
  }

  /**
   *  Add handler to this class.
   *
   *  @param handler a function to handle message.
   */
  public addHandler(handler: Handler) {
    this.handlers.push(handler);
  }
}

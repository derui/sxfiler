type handlerFunction = (ev: MessageEvent) => void;

// This module defines handler of WebSocket.
export default class WebSocketHandler {
  private handlers: handlerFunction[];
  constructor(private readonly ws: WebSocket) {
    this.handlers = [];
  }

  /**
   *  initialize message handler for WebSocket.
   */
  public initialize() {
    this.ws.onmessage = ev => this.handlers.forEach(handler => handler(ev));
  }

  /**
   *  Add handler to this class.
   *
   *  @param handler a function to handle message.
   */
  public addHandler(handler: handlerFunction) {
    this.handlers.push(handler);
  }
}

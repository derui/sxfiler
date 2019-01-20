import * as Common from "./type";
import { Handler } from "./websocket-handler";

type responser = (response: Common.Response) => void;

export default class WebSocketRequester implements Common.Requester, Handler {
  private requestIdMap: { [key: string]: responser } = {};

  /**
   * constructor
   * @param ws available websocket
   */
  constructor(private ws: WebSocket) {}

  /**
   * handle websocket event.
   * @param ev message of websocket
   */
  public handle(ev: MessageEvent) {
    try {
      const json: Common.Response = JSON.parse(ev.data);

      if (json.id && this.requestIdMap[json.id]) {
        const f = this.requestIdMap[json.id];
        delete this.requestIdMap[json.id];
        f(json);
      }
    } catch (e) {
      throw e;
    }
  }

  /**
   * call RPC via Websocket
   * @param request request object
   */
  public async call(request: Common.Request): Promise<Common.Response> {
    let promise: Promise<Common.Response>;
    if (request.id) {
      promise = new Promise(resolve => {
        this.requestIdMap[request.id!] = resolve;
      });
    } else {
      throw new Error("Request ID require when call JSON-RPC");
    }

    this.ws.send(JSON.stringify(request));

    return promise;
  }

  /**
   * send notification to server
   * @param request request object
   */
  public notify(request: Common.Request): void {
    this.ws.send(JSON.stringify(request));
  }
}

import * as Common from "./type";
import { Handler } from "./websocket-handler";
import { ContextLike } from "../../context";

export type NotificationMethod = (context: ContextLike) => (params: any) => void;
export type NotificationMethodMap = {
  [key: string]: NotificationMethod;
};

export default class NotificationServer implements Handler {
  /**
   * constructor
   * @param methodMap available websocket
   */
  constructor(private context: ContextLike, private methodMap: NotificationMethodMap) {}

  /**
   * handle websocket event to handle notification.
   * @param ev message of websocket
   */
  public handle(ev: MessageEvent) {
    const json: Common.Request = JSON.parse(ev.data);

    if (!json.id && this.methodMap[json.method]) {
      this.methodMap[json.method](this.context)(json.params);
    }
  }
}

import * as uuid from "uuid";

// subscriber
export type Subscriber = (message: string | ArrayBuffer) => void;

// unsubscribe a subscriber from Hub
export type Unsubscribe = () => void;

export interface WebSocketHub {
  /**
     subscribe Websocket event. If you want to stop subscription, call function returned from this function
   */
  subscribe(subscriber: Subscriber): Unsubscribe;

  /**
     send data to other side of WebSocket
   */
  send(data: string | ArrayBuffer): void;

  /**
     start event subscritpion
   */
  start(): void;

  /**
     stop event subscritpion
   */
  stop(): void;
}

/**
   create new Hub for ws
 */
export const create = function create(ws: WebSocket): WebSocketHub {
  const subscriberMap = new Map<string, Subscriber>();

  // add event listener
  const listener = (message: MessageEvent) => {
    for (let f of subscriberMap.values()) {
      f(message.data);
    }
  };

  return {
    start() {
      ws.addEventListener("message", listener);
    },

    stop() {
      ws.removeEventListener("message", listener);
    },

    subscribe(subscriber: Subscriber) {
      const id = uuid.v4();

      subscriberMap.set(id, subscriber);

      return () => subscriberMap.delete(id);
    },

    send(data: string | ArrayBuffer) {
      ws.send(data);
    },
  };
};

type Unsubscribe = () => void;
type Handler<EventTypes extends string, Events extends { kind: EventTypes; [key: string]: any }> = (
  event: Events
) => void;

export type SubScriber<EventTypes extends string, Events extends { kind: EventTypes; [key: string]: any }> = {
  subscribe(event: EventTypes, handler: Handler<EventTypes, Events>): Unsubscribe;

  once(event: EventTypes, handler: Handler<EventTypes, Events>): void;
};

export type Publisher<EventTypes extends string, Events extends { kind: EventTypes; [key: string]: any }> = {
  publish(event: Events): void;
};

export type EventHub<EventTypes extends string, Events extends { kind: EventTypes; [key: string]: any }> = SubScriber<
  EventTypes,
  Events
> &
  Publisher<EventTypes, Events>;

/**
 * create an specialized event hub
 */
export const create = <EventTypes extends string, Events extends { kind: EventTypes; [key: string]: any }>(): EventHub<
  EventTypes,
  Events
> => {
  const handlers = new Map<EventTypes, Array<Handler<EventTypes, Events>>>();
  const onceHandlers = new Map<EventTypes, Array<Handler<EventTypes, Events>>>();

  return {
    subscribe(event, handler) {
      const targetHandlers = handlers.get(event) || [];
      targetHandlers.push(handler);
      handlers.set(event, targetHandlers);

      return () => {
        const hs = handlers.get(event);
        handlers.set(event, hs?.filter((v) => v !== handler) || []);
      };
    },

    once(event, handler) {
      const targetHandlers = onceHandlers.get(event) || [];
      targetHandlers.push(handler);
      onceHandlers.set(event, targetHandlers);
    },

    publish(event) {
      const currentOnceHandlers = onceHandlers.get(event.kind) || [];
      onceHandlers.set(event.kind, []);

      handlers.get(event.kind)?.forEach((h) => h(event));
      currentOnceHandlers.forEach((h) => h(event));
    },
  };
};

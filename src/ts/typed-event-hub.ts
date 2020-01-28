import { DecisionRequiredOp, SelectableAction } from "@/modules/decision/reducer";
import * as EventHub from "@/libs/event-hub";

export const EventTypes = {
  FinishDecision: "FinishDecision",
} as const;

export type EventTypes = typeof EventTypes[keyof typeof EventTypes];

type FinishDecisionEvent = {
  kind: "FinishDecision";
  processId: string;
  decisionOp: DecisionRequiredOp;
  resultAction: SelectableAction;
};

export type Events = FinishDecisionEvent;
export const EventCreators = Object.freeze({
  finishDecision(args: Omit<FinishDecisionEvent, "kind">): FinishDecisionEvent {
    return { ...args, kind: EventTypes.FinishDecision };
  },
});

// create new typed actor
export const create = () => EventHub.create<EventTypes, Events>();

export type TypedSubscriber = EventHub.SubScriber<EventTypes, Events>;
export type TypedPublisher = EventHub.Publisher<EventTypes, Events>;
export type Type = EventHub.EventHub<EventTypes, Events>;

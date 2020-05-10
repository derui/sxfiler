import { DecisionRequiredOp, SelectableAction } from "@/modules/decision/reducer";
import * as EventHub from "@/libs/event-hub";
import { ObjectEnum } from "./utils";

export const EventTypes = {
  FinishDecision: "FinishDecision",
  CancelDecision: "CancelDecision",
} as const;
export type EventTypes = ObjectEnum<typeof EventTypes>;

type FinishDecisionEvent = {
  kind: "FinishDecision";
  processId: string;
  decisionOp: DecisionRequiredOp;
  resultAction: SelectableAction;
};
type CancelDecisionEvent = {
  kind: "CancelDecision";
  processId: string;
};

export type Events = FinishDecisionEvent | CancelDecisionEvent;
export const EventCreators = Object.freeze({
  finishDecision(args: Omit<FinishDecisionEvent, "kind">): FinishDecisionEvent {
    return { ...args, kind: EventTypes.FinishDecision };
  },

  cancelDecision(args: { processId: string }): CancelDecisionEvent {
    return { ...args, kind: EventTypes.CancelDecision };
  },
});

// create new typed actor
export const create = () => EventHub.create<EventTypes, Events>();

export type TypedSubscriber = EventHub.SubScriber<EventTypes, Events>;
export type TypedPublisher = EventHub.Publisher<EventTypes, Events>;
export type Type = EventHub.EventHub<EventTypes, Events>;

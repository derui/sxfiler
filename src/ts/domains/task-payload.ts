import { InteractionKind } from "./task-interaction";

export type YesNoPayload = {
  readonly kind: "yes-no";
  readonly payload: boolean;
};

export type StringPayload = {
  readonly kind: "string";
  readonly payload: string;
};

export type IntPayload = {
  readonly kind: "int";
  readonly payload: number;
};

export type InteractionPayloadObject = YesNoPayload | StringPayload | IntPayload;
export type InteractionPayload = InteractionPayloadObject & {
  toServerRepresentation(): any[];
};

const createPayload = (payload: InteractionPayloadObject): InteractionPayload => {
  return {
    ...payload,
    toServerRepresentation() {
      return [payload.kind, payload.payload];
    },
  };
};

/**
 * factory function to create an interaction to send yes/no
 * @param payload
 */
export const createYesNoInteraction = (payload: boolean): InteractionPayload => {
  return createPayload({ kind: InteractionKind.YesNo, payload });
};

/**
 * factory function to create an interaction to send string
 * @param payload
 */
export const createStringInteraction = (payload: string): InteractionPayload => {
  return createPayload({ kind: InteractionKind.String, payload });
};

/**
 * factory function to create an interaction to send number
 * @param payload
 */
export const createIntInteraction = (payload: number): InteractionPayload => {
  return createPayload({ kind: InteractionKind.Int, payload });
};

import { InteractionPayload } from "../domains/task";

// defines API signature for Task group.

export enum Methods {
  SendInteraction = "task/sendInteraction",
}

/**
   API definition for keymap/get
 */
const SendInteraction = {
  method: Methods.SendInteraction,
  parametersTransformer(param: { taskId: string; payload: InteractionPayload }) {
    const { taskId, payload } = param;
    return {
      taskId,
      payload: payload.toServerRepresentation(),
    };
  },
  resultTransformer() {
    return undefined;
  },
};

export const Apis = { SendInteraction };

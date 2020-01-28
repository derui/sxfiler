import { ActionTypes } from "./types";
import { Actions } from "./actions";
import { Configuration } from "@/generated/configuration_pb";

// state of type. Please redefine to what you want.
export type State = {
  configuration: Configuration.AsObject | undefined;
};

export const emptyState: State = { configuration: undefined };

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.UPDATE:
      return Object.freeze({ ...state, configuration: action.payload.config.toObject() });
    default:
      return state;
  }
};

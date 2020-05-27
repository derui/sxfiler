import { ActionTypes } from "./types";
import { Actions } from "./actions";
import { Configuration } from "@/generated/configuration_pb";

// state of type. Please redefine to what you want.
export type State = {
  configuration: { [p: string]: any };
};

export const emptyState: State = { configuration: {} };

const update = function (value: Configuration[]): State {
  return Object.freeze({
    configuration: value.reduce((acc, value) => {
      acc[value.getKeyList().join(".")] = JSON.parse(value.getJsonValue()).value;
      return acc;
    }, {} as { [p: string]: any }),
  });
};

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.UPDATE:
      return update(action.payload.config);
    default:
      return state;
  }
};

import { ActionTypes } from "./types";
import { Actions } from "./actions";
import { Configuration } from "@/generated/configuration_pb";
import { Section, SectionKey } from "@/configurations/types";

export enum DisplayState {
  Editing,
  Closed,
}

// state of type. Please redefine to what you want.
export type State = {
  configuration: { [p: string]: any };
  selectedSection: SectionKey | null;
  displayState: DisplayState;
};

export const emptyState: State = { configuration: {}, selectedSection: null, displayState: DisplayState.Closed };

const update = function (state: State, value: Configuration[]): State {
  return {
    ...state,
    configuration: value.reduce((acc, value) => {
      acc[value.getKeyList().join(".")] = JSON.parse(value.getJsonValue()).value;
      return acc;
    }, {} as { [p: string]: any }),
  };
};

const selectAction = function (state: State, section: Section): State {
  return {
    ...state,
    selectedSection: section.key,
  };
};

const close = function (state: State): State {
  return { ...state, displayState: DisplayState.Closed };
};

const open = function (state: State): State {
  return { ...state, displayState: DisplayState.Editing };
};

export const reducer = (state: State = emptyState, action: Actions): State => {
  switch (action.type) {
    case ActionTypes.UPDATE:
      return update(state, action.payload.config);
    case ActionTypes.SELECT_SECTION:
      return selectAction(state, action.payload.section);
    case ActionTypes.CLOSE:
      return close(state);
    case ActionTypes.OPEN:
      return open(state);
    default:
      return state;
  }
};

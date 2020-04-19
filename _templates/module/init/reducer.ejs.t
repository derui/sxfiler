---
to: src/ts/modules/<%= name %>/reducer.ts
---
import {ActionTypes} from "./types";
import {Actions} from "./actions";

// state of type. Please redefine to what you want.
export type State = {};

export const emptyState: State = {};

export const reducer = (state: State = emptyState, action:Actions): State => {
  switch (action.type) {
    default: return state;
  }
};

---
to: src/ts/modules/<%= name %>/index.ts
---
import { Actions as Actions$ } from "./actions";
import { State as State$ } from "./reducer";

export type Actions = Actions$;
export type State = State$;

export { ActionTypes } from "./types";
export { actions } from "./actions";
export { reducer, emptyState } from "./reducer";
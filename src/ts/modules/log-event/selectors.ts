import { State } from "./reducer";

export const allEvents = (state: State) => Array.from(state.events);

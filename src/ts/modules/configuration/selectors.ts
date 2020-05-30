import { State, DisplayState } from "./reducer";
import { ItemKey } from "@/configurations/types";
import { qualified } from "@/configurations";

export const selectAllKeys = (state: State) => Object.keys(state.configuration);
export const selectAll = (state: State) => Object.entries(state.configuration);

export const selectItem = <T>(state: State, key: ItemKey<T>): T | undefined => state.configuration[qualified(key)];
export const isClosed = (state: State) => state.displayState === DisplayState.Closed;
export const isEditing = (state: State) => state.displayState === DisplayState.Editing;

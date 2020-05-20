import { State } from "./reducer";
import { Key } from "@/configurations";

export const selectAllKeys = (state: State) => Object.keys(state.configuration);
export const selectAll = (state: State) => Object.entries(state.configuration);

export const selectItem = <T>(state: State, key: Key<T>): T | undefined => state.configuration[key.join(".")];

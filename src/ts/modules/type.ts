import { Action } from "redux";

export type AppAction<T extends string, Extra extends {} = {}> = Action<T> & {
  payload: { [K in keyof Extra]: Extra[K] };
};

// generate type for actions with action implementations and action types
export type ActionsType<T extends { [key: string]: string }, ActionCreators extends object> = {
  [key in keyof ActionCreators]: ActionCreators[key] extends (...args: any[]) => AppAction<T[keyof T], any>
    ? ReturnType<ActionCreators[key]>
    : never;
}[keyof ActionCreators];

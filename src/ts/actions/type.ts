import { Action } from "redux";

export type AppAction<T extends string, Extra extends {} = {}> = Action<T> & { [K in keyof Extra]: Extra[K] };

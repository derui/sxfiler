// dispatcher function to use in UseCase
export type dispatcher<Action> = (action: Action) => void;

// dispatcher interface to store in context.
export interface Dispatcher<Action> {
  dispatch(action: Action): void;
}

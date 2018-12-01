// dispatcher function to use in UseCase
export type dispatcher<Action> = (action: Action) => void;

// dispatcher interface to store in context.
export interface Dispatcher<Action> {
  dispatch(action: Action): void;
}

// type of subscriber
export type Subscriber<Action> = (payload: Action) => void;

// interface of observer
export interface Observer<T> {
  /**
   * add subscription handler
   * @param subscriber subscription function
   */
  subscribe(subscriber: Subscriber<T>): void;

  /**
   * unsubscribe the subscription handler
   * @param subscriber subscription function to delete
   */
  unsubscribe(subscriber: Subscriber<T>): void;
}

// interface of UseCase
export interface UseCase<Param, Result> {
  /**
   * execute usecase with parameter.
   * @param param parameter object
   */
  execute(param: Param): Promise<Result>;
}

export { StoreState } from "./store-state";

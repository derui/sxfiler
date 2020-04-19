import { Actions, State } from "@/modules";
import { ClientResolverLike } from "./client-resolver";
import { Dispatcher } from "@/types";

/**
 * common state that is passing to command
 */
export type CommandState = {
  state: State;
  clientResolver: ClientResolverLike;
};

/**
 * Define use case that to use asynchrnous in use case
 */
export type CommandLike<T = undefined> = {
  /**
   * identifier of command.
   */
  readonly identifier: string;

  /**
   * execute usecase with argument
   * @param arg
   */
  execute(dispatch: Dispatcher<Actions>, arg: CommandState, payload: T): Promise<void>;
};

/**
 * descriptor of command
 */
export type CommandDescriptor<T> = {
  readonly identifier: string;
  readonly payloadSpec: T;
};

/**
 * type of command resolver
 */
export type CommandFactory<T> = () => CommandLike<T>;

/**
 * define resolver set of commands.
 */
export type DescriptorsType<T> = {
  [K in keyof T]: T[K] extends CommandDescriptor<infer Payload> ? CommandDescriptor<Payload> : never;
};

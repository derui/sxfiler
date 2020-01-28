import { Actions } from "@/modules";
import * as types from "@/types";
import { CommandLike, CommandState } from "@/commands/type";

// interface of context
export type ContextLike = {
  use<Param>(useCase: CommandLike<Param>): CommandExecutor<Param>;
};

export type CommandExecutor<Param> = (state: CommandState, payload: Param) => Promise<void>;

// Context for application
class Context implements ContextLike {
  public readonly dispatcher: types.Dispatcher<Actions>;

  /**
   * constructor of context
   * @param dispatcher dispatcher
   */
  public constructor(dispatcher: types.Dispatcher<Actions>) {
    this.dispatcher = dispatcher;
  }

  /**
   * Register a use case to use next execution
   * @param command a use case to execute with this context
   * @return executor for the use case
   */
  public use<Param>(command: CommandLike<Param>): CommandExecutor<Param> {
    const dispatcter = this.dispatcher;
    return async (param, payload) => {
      command.execute(dispatcter, param, payload);
    };
  }
}

/**
 * factory of Context
 */
export const createContext = ({ dispatcher }: { dispatcher: types.Dispatcher<Actions> }): ContextLike => {
  return new Context(dispatcher);
};

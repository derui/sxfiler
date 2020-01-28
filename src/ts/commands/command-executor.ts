import { CommandFactory } from "./type";
import { ContextLike } from "@/context";
import { ClientResolverLike } from "./client-resolver";
import { State } from "@/modules";

export type Type = {
  /**
   * execute the command that is named to fqdn with parameter
   */
  execute<T>(factory: CommandFactory<T>, arg: State, payload: T): Promise<void>;
};

type TypeInner = Type & {
  _resolver: ClientResolverLike;
};

/**
 * Create new instance of CommandRegistrar
 */
export const create = (resolver: ClientResolverLike, context: ContextLike): Type => {
  return {
    _resolver: resolver,

    execute<T>(factory: CommandFactory<T>, arg: State, payload: T) {
      return context.use(factory())({ state: arg, clientResolver: this._resolver }, payload);
    },
  } as TypeInner;
};

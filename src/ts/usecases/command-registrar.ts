import { CommandLike } from "@/usecases/type";
import { Client } from "@/libs/json-rpc/client";
import { ApiMethod } from "@/apis";
import { AppState } from "@/states";
import { ContextLike } from "@/context";

export type RegisterArg = {
  moduleId: string;
  commandId: string;
  commandInstance: CommandLike;
};

export type CommandRegistrar = {
  /**
   * Regist an instance of commands
   */
  regist(arg: RegisterArg): CommandRegistrar;

  /**
   * execute the command that is named to fqdn with parameter
   */
  execute(fqdn: string, context: ContextLike, arg: { state: AppState }): void;
};

type CommandRegistrarInner = CommandRegistrar & {
  _client: Client<ApiMethod>;
  _commands: { [key: string]: CommandLike };
  /**
   * Find the command that have the given name
   */
  findCommand(fqdn: string): CommandLike | undefined;
};

/**
 * Create new instance of CommandRegistrar
 */
export const createCommandRegistrar = (
  client: Client<ApiMethod>,
  commands: { [key: string]: CommandLike } = {}
): CommandRegistrar => {
  return {
    _client: client,
    _commands: commands,

    regist({ moduleId, commandId, commandInstance }: RegisterArg): CommandRegistrar {
      return createCommandRegistrar(this._client, { ...this._commands, [`${moduleId}.${commandId}`]: commandInstance });
    },
    findCommand(fqdn: string) {
      return this._commands[fqdn];
    },
    execute(fqdn: string, context: ContextLike, arg: { state: AppState }) {
      const command = this.findCommand(fqdn);

      if (!command) {
        return;
      }

      context.use(command).execute({ ...arg, client: this._client });
    },
  } as CommandRegistrarInner;
};

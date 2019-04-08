import { CommandLike } from "../usecases/type";

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
   * Find the command that have the given name
   */
  findCommand(fqdn: string): CommandLike | undefined;
};

type CommandRegistrarInner = CommandRegistrar & {
  _commands: { [key: string]: CommandLike };
};

/**
 * Create new instance of CommandRegistrar
 */
export const createCommandRegistrar = (commands: { [key: string]: CommandLike } = {}): CommandRegistrar => {
  return {
    _commands: commands,

    regist({ moduleId, commandId, commandInstance }: RegisterArg): CommandRegistrar {
      return createCommandRegistrar({ ...this._commands, [`${moduleId}.${commandId}`]: commandInstance });
    },
    findCommand(fqdn: string) {
      return this._commands[fqdn];
    },
  } as CommandRegistrarInner;
};

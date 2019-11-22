import { Actions } from "@/actions";
import { CommandLike } from "../type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "../command-registrar";

const belongingModuleId = "builtin";
const commandId = "quit";

/**
 * Regist command instance to the registrar
 */
export const registCommand = function registCommand(registrar: CommandRegistrar) {
  return registrar.regist({
    moduleId: belongingModuleId,
    commandId,
    commandInstance: createCommand(),
  });
};

export const createCommand = function createCommand(): CommandLike {
  return {
    async execute(_: Dispatcher<Actions>, args) {
      if (!args) {
        throw new Error("Do not take store state");
      }

      args.clientResolver.appClient().quit();
    },
  };
};

import { Actions } from "@/actions";
import { actions } from "@/actions/filer";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";

const belongingModuleId = "builtin";
const commandId = "filer.changePaneSide";

/**
 * Regist command instance to the registrar
 */
export const registCommand = function registCommand(registrar: CommandRegistrar) {
  registrar.regist({
    moduleId: belongingModuleId,
    commandId,
    commandInstance: createCommand(),
  });
};

export const createCommand = function createCommand(): CommandLike {
  return {
    async execute(dispatcher: Dispatcher<Actions>) {
      dispatcher.dispatch(actions.changeSide());
    },
  };
};

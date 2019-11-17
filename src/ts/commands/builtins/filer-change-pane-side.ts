import { Actions } from "@/actions";
import * as actions from "@/actions/filer";
import { CommandLike } from "../type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "../command-registrar";

const belongingModuleId = "builtin";
const commandId = "filer.changePaneSide";

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
    async execute(dispatcher: Dispatcher<Actions>) {
      dispatcher.dispatch(actions.changeSide());
    },
  };
};

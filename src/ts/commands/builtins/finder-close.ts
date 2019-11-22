import { Actions } from "@/actions";
import * as actions from "@/actions/completer";
import { CommandLike } from "../type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "../command-registrar";
import { UIContext } from "@/types/ui-context";

const belongingModuleId = "builtin";
const commandId = "completer.close";

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
    async execute(dispatch: Dispatcher<Actions>, args) {
      if (!args) {
        throw new Error("Do not take store state");
      }

      dispatch.dispatch(actions.close(UIContext.ForFinder));
    },
  };
};
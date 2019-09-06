import { Actions } from "@/actions";
import * as completerActions from "@/actions/completer";
import * as filerActions from "@/actions/filer";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { currentSelectedCandidate } from "@/states/completer";
import { UIContext } from "@/types/ui-context";

const belongingModuleId = "builtin";
const commandId = "finder.select";

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
      const { state } = args;

      const currentNode = currentSelectedCandidate(state.completer);

      if (!currentNode) {
        console.log("Did not select any node in completer");
        return;
      }

      const side = state.fileList.currentSide;

      dispatch.dispatch(completerActions.close(UIContext.ForFinder));
      dispatch.dispatch(filerActions.select(side, currentNode.id));
    },
  };
};

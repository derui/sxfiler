import { Actions } from "@/actions";
import * as actions from "@/actions/filer";
import * as completerActions from "@/actions/completer";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis";
import { currentSelectedCandidate } from "@/states/completer";
import { UIContext } from "@/types/ui-context";

const belongingModuleId = "builtin";
const commandId = "history.select";

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
      const { state, client } = args;

      const currentNode = currentSelectedCandidate(state.completer);

      dispatch.dispatch(completerActions.close(UIContext.ForHistory));

      if (!currentNode) {
        return;
      }
      const side = state.fileList.currentSide;

      const filer = await client.call(Apis.Filer.Jump, {
        name: side,
        location: currentNode.value,
      });

      if (!filer) {
        return;
      }

      dispatch.dispatch(actions.load({ filer }));
    },
  };
};

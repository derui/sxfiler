import { Actions } from "@/actions";
import * as actions from "@/actions/filer";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis";
import { currentFocusingNode } from "@/states/file-list";

const belongingModuleId = "builtin";
const commandId = "filer.enterDirectory";

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

      const currentNode = currentFocusingNode(state.fileList);
      if (!currentNode || currentNode.stat.isFile) {
        return Promise.resolve();
      }

      const side = state.fileList.currentSide;
      const filer = await client.call(Apis.Filer.EnterDirectory, {
        name: side,
        itemId: currentNode.id,
      });

      if (!filer) {
        return;
      }

      dispatch.dispatch(actions.load({ filer }));
    },
  };
};

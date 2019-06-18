import { Actions } from "../../actions";
import { actions } from "../../actions/filer";
import { CommandLike } from "../type";
import { Dispatcher } from "../../types";
import { CommandRegistrar } from "../command-registrar";
import { Apis } from "../../apis";
import { currentFocusingNode } from "../../states/file-list";

const belongingModuleId = "builtin";
const commandId = "filer.enterDirectory";

/**
 * Regist command instance to the registrar
 */
export const registCommand = (registrar: CommandRegistrar) =>
  registrar.regist({
    moduleId: belongingModuleId,
    commandId,
    commandInstance: createCommand(),
  });

export const createCommand = (): CommandLike => {
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
        nodeId: currentNode.id,
      });

      if (!filer) {
        return;
      }

      dispatch.dispatch(actions.load({ filer }));
    },
  };
};

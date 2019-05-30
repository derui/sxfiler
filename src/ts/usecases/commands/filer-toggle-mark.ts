import { Actions } from "../../actions";
import { actions } from "../../actions/filer";
import { CommandLike } from "../type";
import { Dispatcher } from "../../types";
import { CommandRegistrar } from "../command-registrar";
import { Apis } from "../../apis";
import * as FileListState from "../../states/file-list";

const belongingModuleId = "builtin";
const commandId = "filer.toggleMark";

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

      const side = state.fileList.currentSide;
      const focused = FileListState.currentFocusingNode(state.fileList);

      if (!focused) {
        console.log("not found focused node");
        return Promise.resolve();
      }

      const filer = await client.call(Apis.Filer.ToggleMark, { name: side, nodeIds: [focused.id] });

      if (!filer) {
        return;
      }

      dispatch.dispatch(
        actions.load({
          side,
          filer,
        })
      );
    },
  };
};
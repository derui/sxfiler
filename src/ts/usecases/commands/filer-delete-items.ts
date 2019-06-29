import { Actions } from "../../actions";
import { CommandLike } from "../type";
import { Dispatcher } from "../../types";
import { CommandRegistrar } from "../command-registrar";
import { Apis } from "../../apis";
import * as FileListState from "../../states/file-list";

const belongingModuleId = "builtin";
const commandId = "filer.delete";

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
    async execute(_: Dispatcher<Actions>, args) {
      if (!args) {
        throw new Error("Do not take store state");
      }
      const { state, client } = args;

      const side = state.fileList.currentSide;
      const fileTree = FileListState.filerOnSide(state.fileList, side);

      if (!fileTree) {
        console.log("not found file tree");
        return Promise.resolve();
      }

      await client.call(Apis.Filer.Delete, {
        source: side,
        itemIds: fileTree.markedItems.map(v => v.id),
      });
    },
  };
};
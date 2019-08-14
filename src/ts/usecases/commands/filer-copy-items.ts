import { Actions } from "@/actions";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis";
import * as FileListState from "@/states/file-list";

const belongingModuleId = "builtin";
const commandId = "filer.copy";

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
      const { state, client } = args;

      const side = state.fileList.currentSide;
      const fileTree = FileListState.filerOnSide(state.fileList, side);

      if (!fileTree) {
        console.log("not found file tree");
        return Promise.resolve();
      }

      await client.call(Apis.Filer.Copy, {
        source: side,
        dest: FileListState.swapSide(side),
        itemIds: fileTree.markedItems.map(v => v.id),
      });
    },
  };
};

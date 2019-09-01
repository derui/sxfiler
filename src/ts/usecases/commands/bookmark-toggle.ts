import { Actions } from "@/actions";
import * as actions from "@/actions/bookmark";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { currentFocusingNode, findBookmark } from "@/states/file-list";
import { Apis } from "@/apis/bookmark-api";

const belongingModuleId = "builtin";
const commandId = "bookmark.register";

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
    async execute(dispatcher: Dispatcher<Actions>, state) {
      if (!state) {
        throw Error("state must not be empty");
      }

      const { state: appState } = state;
      const node = currentFocusingNode(appState.fileList);
      if (!node) {
        return;
      }

      let bookmark = findBookmark(node)(appState.fileList);
      if (!bookmark) {
        bookmark = await state.client.call(Apis.Register, { path: node.fullPath });
        dispatcher.dispatch(actions.registerBookmark(bookmark));
      } else {
        bookmark = await state.client.call(Apis.Delete, { id: bookmark.id });
        dispatcher.dispatch(actions.deleteBookmark(bookmark));
      }
    },
  };
};

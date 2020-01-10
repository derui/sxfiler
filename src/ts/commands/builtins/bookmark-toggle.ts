import { Actions } from "@/actions";
import * as actions from "@/actions/bookmark";
import { CommandLike } from "../type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "../command-registrar";
import { currentFocusingNode, findBookmark } from "@/states/file-list";
import { Apis } from "@/apis/bookmark-api";

const belongingModuleId = "builtin";
const commandId = "bookmark.toggle";

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
    async execute(dispatcher: Dispatcher<Actions>, args) {
      if (!args) {
        throw Error("state must not be empty");
      }

      const { state: appState } = args;
      const node = currentFocusingNode(appState.fileList);
      if (!node) {
        return;
      }

      let bookmark = findBookmark(node)(appState.fileList);
      const apiClient = args.clientResolver.apiClient();
      if (!bookmark) {
        bookmark = await apiClient.call(Apis.Register, node.fullPath);
        dispatcher.dispatch(actions.registerBookmark(bookmark));
      } else {
        bookmark = await apiClient.call(Apis.Delete, bookmark.id);
        dispatcher.dispatch(actions.deleteBookmark(bookmark));
      }
    },
  };
};

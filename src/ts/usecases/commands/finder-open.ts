import { Actions } from "@/actions";
import * as actions from "@/actions/completer";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis";
import * as FileListState from "@/states/file-list";
import { createCandidate } from "@/domains/candidate";
import { UIContext } from "@/types/ui-context";

const belongingModuleId = "builtin";
const commandId = "finder.open";

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

      const side = state.fileList.currentSide;
      const focused = FileListState.filerOnSide(state.fileList, side);

      if (!focused) {
        console.log("not found focused node");
        return Promise.resolve();
      }

      const source = focused.items.map(v => createCandidate({ id: v.id, value: v.name }));
      dispatch.dispatch(actions.open("Find Item", UIContext.ForFinder));

      await client.call(Apis.Completion.Setup, { source });
      dispatch.dispatch(actions.replaceCandidates(source));
    },
  };
};

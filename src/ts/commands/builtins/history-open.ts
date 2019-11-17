import { Actions } from "@/actions";
import * as actions from "@/actions/completer";
import { CommandLike } from "../type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "../command-registrar";
import { Apis } from "@/apis";
import * as FileListState from "@/states/file-list";
import { createCandidate } from "@/domains/candidate";
import { UIContext } from "@/types/ui-context";

const belongingModuleId = "builtin";
const commandId = "history.open";

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
      const { state, clientResolver } = args;

      const side = state.fileList.currentSide;
      const focused = FileListState.filerOnSide(state.fileList, side);

      if (!focused) {
        console.log("not found focused node");
        return Promise.resolve();
      }

      const source = focused.history.records.map(v => createCandidate({ id: v.location, value: v.location }));
      dispatch.dispatch(actions.open("History", UIContext.ForHistory));

      await clientResolver.apiClient().call(Apis.Completion.Setup, { source });
      dispatch.dispatch(actions.replaceCandidates(source));
    },
  };
};

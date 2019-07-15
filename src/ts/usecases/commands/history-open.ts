import { Actions } from "@/actions";
import * as actions from "@/actions/history";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis";
import * as FileListState from "@/states/file-list";
import { createCandidate } from "@/domains/candidate";

const belongingModuleId = "builtin";
const commandId = "history.open";

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
      const focused = FileListState.filerOnSide(state.fileList, side);

      if (!focused) {
        console.log("not found focused node");
        return Promise.resolve();
      }

      const source = focused.history.records.map(v => createCandidate({ id: v.location, value: v.location }));
      dispatch.dispatch(actions.open(side));

      await client.call(Apis.Completion.Setup, { source: source.map(v => v.plain()) });
      dispatch.dispatch(actions.replaceCandidates(source));
    },
  };
};

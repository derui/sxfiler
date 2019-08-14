import { Actions } from "@/actions";
import { actions } from "@/actions/filer";
import * as historyActions from "@/actions/history";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis";
import { currentSelectedCandidate } from "@/states/history";

const belongingModuleId = "builtin";
const commandId = "history.select";

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

      const currentNode = currentSelectedCandidate(state.history);

      dispatch.dispatch(historyActions.close());

      if (!currentNode) {
        return;
      }

      const filer = await client.call(Apis.Filer.Jump, {
        name: state.history.side,
        location: currentNode.value,
      });

      if (!filer) {
        return;
      }

      dispatch.dispatch(actions.load({ filer }));
    },
  };
};

import { Actions } from "@/actions";
import * as actions from "@/actions/task";
import { CommandLike } from "../type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "../command-registrar";
import { Apis } from "@/apis/task-api";

const belongingModuleId = "builtin";
const commandId = "task.cancel";

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

      const taskId = state.taskInteraction.currentTaskId;
      if (!taskId) {
        return;
      }

      await clientResolver.apiClient().call(Apis.Cancel, taskId);

      dispatch.dispatch(actions.canceled(taskId));
    },
  };
};

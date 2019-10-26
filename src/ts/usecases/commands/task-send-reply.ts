import { Actions } from "@/actions";
import * as actions from "@/actions/task";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis/task-api";

const belongingModuleId = "builtin";
const commandId = "task.sendReply";

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

      const reply = state.taskInteraction.currentReply();
      if (!reply) {
        return;
      }

      await client.call(Apis.SendReply, reply);

      dispatch.dispatch(actions.sendReply(reply));
    },
  };
};

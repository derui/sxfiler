import { Actions } from "@/actions";
import * as actions from "@/actions/task";
import { CommandLike } from "../type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "../command-registrar";

const belongingModuleId = "builtin";
const commandId = "task.selectPreviousReply";

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

      const currentIndex = args.state.taskInteraction.currentReplyIndex;
      if (currentIndex === undefined) {
        return;
      }

      dispatch.dispatch(actions.selectReply(currentIndex - 1));
    },
  };
};

import { Actions } from "@/actions";
import { actions } from "@/actions/filer";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { filerOnSide } from "@/states/file-list";
import { Direction } from "@/domains/filer";
import { CommandRegistrar } from "@/usecases/command-registrar";

const belongingModuleId = "builtin";
const commandId = "filer.moveCursorUp";

/**
 * Regist command instance to the registrar
 */
export const registCommand = function registCommand(registrar: CommandRegistrar) {
  registrar.regist({
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
      const { state } = args;

      const side = state.fileList.currentSide;
      const filer = filerOnSide(state.fileList, side);

      if (!filer) {
        return;
      }

      dispatch.dispatch(actions.update({ filer: filer.moveIndex(Direction.Up) }));
    },
  };
};

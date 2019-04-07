import { Actions } from "../../actions";
import { actions } from "../../actions/filer";
import { CommandLike } from "../type";
import { Dispatcher } from "../../types";
import { filerOnSide } from "../../states/file-list";
import { Direction } from "../../domains/filer";

export const createCommand = (): CommandLike => {
  return {
    execute(dispatch: Dispatcher<Actions>, args) {
      if (!args) {
        throw new Error("Do not take store state");
      }
      const { state } = args;

      const side = state.fileList.currentSide;
      const filer = filerOnSide(state.fileList, side);

      if (!filer) {
        return;
      }

      dispatch.dispatch(
        actions.update({
          side,
          filer: filer.moveIndex(Direction.Up),
        })
      );
    },
  };
};

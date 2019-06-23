// defines use case to update a filer from notification
import { Actions } from "../../actions";
import { actions } from "../../actions/filer";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";
import { Filer } from "../../domains/filer";

type Args = {
  filer: Filer;
};
export type UseCase = UseCaseLike<Actions, Args>;

export const createUseCase = (): UseCase => {
  return {
    execute(dispatcher: Dispatcher<Actions>, { filer }: Args) {
      dispatcher.dispatch(actions.update({ filer }));
    },
  };
};

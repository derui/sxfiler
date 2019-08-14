// defines use case to update a filer from notification
import { Actions } from "@/actions";
import * as actions from "@/actions/filer";
import { Dispatcher } from "@/types";
import { UseCaseLike } from "@/usecases/type";
import { Filer } from "@/domains/filer";

type Args = {
  filer: Filer;
};
export type UseCase = UseCaseLike<Actions, Args>;

export const createUseCase = function createUseCase(): UseCase {
  return {
    execute(dispatcher: Dispatcher<Actions>, { filer }: Args) {
      dispatcher.dispatch(actions.load({ filer }));
    },
  };
};

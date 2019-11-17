import { Actions } from "@/actions";
import { AppState } from "@/states";
import { AsyncUseCaseLike } from "@/usecases/type";
import { ClientResolverLike } from "./client-resolver";

/**
 * A type of commands in this application.
 */
export type CommandLike = AsyncUseCaseLike<
  Actions,
  {
    state: AppState;
    clientResolver: ClientResolverLike;
  }
>;

import { Actions } from "../../actions";
import { actions } from "../../actions/notification";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";

interface Arg {
  notificationId: string;
}

// An use case to remove notification from system
export default class RemoveUseCase implements UseCaseLike<Actions, Arg> {
  public execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
    const { notificationId } = arg;
    dispatcher.dispatch(actions.remove(notificationId));
  }
}

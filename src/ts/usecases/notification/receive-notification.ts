import { Actions } from "../../actions";
import { actions } from "../../actions/notification";
import { Notification } from "../../domains/notification";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";

interface Arg {
  notification: Notification;
}

export default class ReceiveNotificationUseCase implements UseCaseLike<Actions, Arg> {
  public execute(dispatcher: Dispatcher<Actions>, arg: Arg) {
    const { notification } = arg;
    dispatcher.dispatch(actions.receiveNotification(notification));
  }
}

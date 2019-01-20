import { Actions } from "../../actions";
import { actions } from "../../actions/ui-context";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";

export default class UseCase implements UseCaseLike<Actions> {
  public execute(dispatcher: Dispatcher<Actions>) {
    dispatcher.dispatch(actions.enablePreview());
  }
}

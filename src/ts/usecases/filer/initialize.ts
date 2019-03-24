// defines use case to initialize filer
import { Actions } from "../../actions";
import { actions } from "../../actions/filer";
import { Dispatcher } from "../../types";
import { UseCaseLike } from "../type";
import { Client } from "../../libs/json-rpc/client";
import { ApiMethod } from "../../apis";
import { Side } from "../../types/store-state/file-list";
import { Apis } from "../../apis";

export default class UseCase implements UseCaseLike<Actions, { location: string }> {
  private client: Client<ApiMethod>;
  constructor(client: Client<ApiMethod>) {
    this.client = client;
  }

  public async execute(dispatcher: Dispatcher<Actions>, { location }: { location: string }) {
    const initialLocation = location;

    let leftFiler = await this.client.call(Apis.Filer.Get, Side.Left);
    if (!leftFiler) {
      leftFiler = await this.client.call(Apis.Filer.Make, { initialLocation, name: Side.Left });
    }

    let rightFiler = await this.client.call(Apis.Filer.Get, Side.Right);
    if (!rightFiler) {
      rightFiler = await this.client.call(Apis.Filer.Make, { initialLocation, name: Side.Right });
    }

    return dispatcher.dispatch(actions.initialize({ left: leftFiler, right: rightFiler }));
  }
}

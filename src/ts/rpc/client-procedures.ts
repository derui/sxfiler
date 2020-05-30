import { Request, Command, Status, CommandMap } from "@/generated/service_pb";
import {
  InitializeRequest,
  InitializeResponse,
  ReloadAllRequest,
  ReloadAllResponse,
  MoveLocationRequest,
  MoveLocationResponse,
  OpenFileItemRequest,
  OpenFileItemResponse,
  UpDirectoryRequest,
  UpDirectoryResponse,
  ToggleMarkOfItemRequest,
  ToggleMarkOfItemResponse,
  MoveRequest,
  MoveResponse,
  CopyRequest,
  CopyResponse,
  DeleteRequest,
  DeleteResponse,
} from "@/generated/filer_pb";
import * as uuid from "uuid";
import { ProcedureDef } from "./client";
import { also } from "@/libs/fn";
import { RPCClient } from "@/libs/websocket-rpc/client";
import { GetRequest, GetResponse } from "@/generated/keymap_pb";
import * as Con from "@/generated/configuration_pb";
import * as CompleterPb from "@/generated/completer_pb";

/**
 * make Request with payload and command
 */
const makeRequest = (command: CommandMap[keyof CommandMap], payload: Uint8Array): Request => {
  const req = also(new Request(), (v) => {
    v.setId(uuid.v4());
    v.setCommand(command);
    v.setPayload(payload);
  });
  return req;
};

type encoder<T> = (req: T) => Uint8Array;

type decoder<T> = (v: Uint8Array) => T;

/**
 * generic function to call an procedure
 */
const callGeneric = <Req, Res>(
  command: CommandMap[keyof CommandMap],
  encoder: encoder<Req>,
  decoder: decoder<Res>
): ((client: RPCClient, request: Req) => Promise<Res>) => {
  return async function (client: RPCClient, request: Req): Promise<Res> {
    const payload = encoder(request);
    const req = makeRequest(command, payload);
    const res = await client.send(req);

    if (res.getStatus() === Status.SUCCESS) {
      return decoder(res.getPayload_asU8());
    }
    throw new Error("Command failed");
  };
};

export namespace Filer {
  /**
   * procedure for `Command.FilerInitialize`
   */
  export const initialize: ProcedureDef<InitializeRequest, InitializeResponse> = callGeneric(
    Command.FILER_INITIALIZE,
    (v) => v.serializeBinary(),
    InitializeResponse.deserializeBinary
  );

  /**
   * procedure for `Command.FilerReloadAll`
   */
  export const reloadAll: ProcedureDef<ReloadAllRequest, ReloadAllResponse> = callGeneric(
    Command.FILER_RELOAD_ALL,
    (v) => v.serializeBinary(),
    ReloadAllResponse.deserializeBinary
  );

  /**
   * procedure for `Command.FilerMoveLocation`
   */
  export const moveLocation: ProcedureDef<MoveLocationRequest, MoveLocationResponse> = callGeneric(
    Command.FILER_MOVE_LOCATION,
    (v) => v.serializeBinary(),
    MoveLocationResponse.deserializeBinary
  );

  /**
   * procedure for `Command.FileOpenFileItem`
   */
  export const openFileItem: ProcedureDef<OpenFileItemRequest, OpenFileItemResponse> = callGeneric(
    Command.FILER_OPEN_FILE_ITEM,
    (v) => v.serializeBinary(),
    OpenFileItemResponse.deserializeBinary
  );

  /**
   * procedure for `Command.UpDirectory`
   */
  export const upDirectory: ProcedureDef<UpDirectoryRequest, UpDirectoryResponse> = callGeneric(
    Command.FILER_UP_DIRECTORY,
    (v) => v.serializeBinary(),
    UpDirectoryResponse.deserializeBinary
  );

  /**
   * procedure for `Command.ToggleMark`
   */
  export const toggleMark: ProcedureDef<ToggleMarkOfItemRequest, ToggleMarkOfItemResponse> = callGeneric(
    Command.FILER_TOGGLE_MARK_OF_ITEM,
    (v) => v.serializeBinary(),
    ToggleMarkOfItemResponse.deserializeBinary
  );

  export const moveItems: ProcedureDef<MoveRequest, MoveResponse> = callGeneric(
    Command.FILER_MOVE,
    (v) => v.serializeBinary(),
    MoveResponse.deserializeBinary
  );

  export const copyItems: ProcedureDef<CopyRequest, CopyResponse> = callGeneric(
    Command.FILER_COPY,
    (v) => v.serializeBinary(),
    CopyResponse.deserializeBinary
  );

  export const deleteItems: ProcedureDef<DeleteRequest, DeleteResponse> = callGeneric(
    Command.FILER_DELETE,
    (v) => v.serializeBinary(),
    DeleteResponse.deserializeBinary
  );
}

export namespace Keymap {
  /**
   * procedure for `Command.KeymapGet`
   */
  export const getKeymap: ProcedureDef<GetRequest, GetResponse> = callGeneric(
    Command.KEYMAP_GET,
    (v) => v.serializeBinary(),
    GetResponse.deserializeBinary
  );
}

export namespace Configuration {
  export const getConfiguration: ProcedureDef<Con.GetRequest, Con.GetResponse> = callGeneric(
    Command.CONFIGURATION_GET,
    (v) => v.serializeBinary(),
    Con.GetResponse.deserializeBinary
  );

  export const updateConfiguration: ProcedureDef<Con.UpdateRequest, Con.UpdateResponse> = callGeneric(
    Command.CONFIGURATION_UPDATE,
    (v) => v.serializeBinary(),
    Con.UpdateResponse.deserializeBinary
  );
}

export namespace Completer {
  export const initialize: ProcedureDef<CompleterPb.InitializeRequest, CompleterPb.InitializeResponse> = callGeneric(
    Command.COMPLETER_INITIALIZE,
    (v) => v.serializeBinary(),
    CompleterPb.InitializeResponse.deserializeBinary
  );

  export const complete: ProcedureDef<CompleterPb.CompleteRequest, CompleterPb.CompleteResponse> = callGeneric(
    Command.COMPLETER_COMPLETE,
    (v) => v.serializeBinary(),
    CompleterPb.CompleteResponse.deserializeBinary
  );
}

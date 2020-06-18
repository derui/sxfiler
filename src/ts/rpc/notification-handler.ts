import { Command } from "@/generated/service_pb";
import {
  UpdatedNotificationResponse,
  UpdatedNotificationRequest,
  CopyUserDecisionRequest,
  CopyUserDecisionResponse,
  Action,
  UpdatedFileWindowNotificationRequest,
  Side as SideMap,
  UpdatedFileWindowNotificationResponse,
  MoveUserDecisionRequest,
  MoveUserDecisionResponse,
  DeleteUserDecisionRequest,
  DeleteUserDecisionResponse,
} from "@/generated/filer_pb";
import { State } from "@/modules";
import { descriptors } from "@/commands/internal";
import * as CommandExecutor from "@/commands/command-executor";
import * as CommandResolver from "@/commands/command-resolver";
import * as winston from "winston";
import { Loggers } from "@/loggers";
import { DecisionRequiredOp, DecisionAction } from "@/modules/decision/reducer";
import { ResponseSender, ProcessResult } from "@/libs/websocket-rpc/server";
import { TypedSubscriber, EventTypes } from "@/typed-event-hub";
import { Side } from "@/modules/filer/reducer";
import { CompletionResultNotificationRequest, CompletionResultNotificationResponse } from "@/generated/completer_pb";
import * as Con from "@/generated/configuration_pb";

/**
 * this module provides handler to handle commands that are notifications sent from server.
 */

/**
 * factory constructor arguments
 */
export type Args = {
  getState: () => State;
  getCommandResolver: () => CommandResolver.Type;
  getCommandExecutor: () => CommandExecutor.Type;
  getSubscriber: () => TypedSubscriber;
};

type InnerCommandHandler = (args: Args, id: string, payload: Uint8Array, lazyResponse: ResponseSender) => void;

/**
 * handler for `CopyUserDecisionRequest`
 */
const handleCopyUserDecision: InnerCommandHandler = (args, id, payload, lazyResponse) => {
  const logger = winston.loggers.get(Loggers.RPC);
  const request = CopyUserDecisionRequest.deserializeBinary(payload);
  const factory = args.getCommandResolver().resolveBy(descriptors.decisionRequest);
  const reset = args.getCommandResolver().resolveBy(descriptors.decisionReset);
  const item = request.getItem();
  if (!factory || !item || !reset) {
    logger.error(`Invalid path: ${descriptors.filerUpdate.identifier}`);
    lazyResponse(ProcessResult.ignored());
    return;
  }

  logger.info("Start handling event request decision for copy");

  args.getCommandExecutor().execute(factory, args.getState(), {
    requiredOp: DecisionRequiredOp.Copy,
    fileItem: item,
    processId: id,
  });
  const unsubscribe = args.getSubscriber().subscribe(EventTypes.FinishDecision, (e) => {
    const response = new CopyUserDecisionResponse();
    switch (e.kind) {
      case EventTypes.CancelDecision: {
        if (e.processId !== id) {
          return;
        }

        response.setAction(Action.CANCEL);
        break;
      }
      case EventTypes.FinishDecision: {
        if (e.processId !== id) {
          return;
        }

        switch (e.resultAction.kind) {
          case DecisionAction.Overwrite:
            response.setAction(Action.OVERWRITE);
            break;
          case DecisionAction.Rename:
            response.setAction(Action.RENAME);
            response.setNewName(e.resultAction.newName);
            break;
        }
      }
    }

    lazyResponse(ProcessResult.successed(response.serializeBinary()));
    unsubscribe();
    args.getCommandExecutor().execute(reset, args.getState(), undefined);
  });
};

/**
 * handler for `MoveUserDecisionRequest`
 */
const handleMoveUserDecision: InnerCommandHandler = (args, id, payload, lazyResponse) => {
  const logger = winston.loggers.get(Loggers.RPC);
  const request = MoveUserDecisionRequest.deserializeBinary(payload);
  const factory = args.getCommandResolver().resolveBy(descriptors.decisionRequest);
  const reset = args.getCommandResolver().resolveBy(descriptors.decisionReset);
  const item = request.getItem();
  if (!factory || !item || !reset) {
    logger.error(`Invalid path: ${descriptors.filerUpdate.identifier}`);
    lazyResponse(ProcessResult.ignored());
    return;
  }

  logger.info("Start handling event request decision for move");

  args.getCommandExecutor().execute(factory, args.getState(), {
    requiredOp: DecisionRequiredOp.Move,
    fileItem: item,
    processId: id,
  });

  const unsubscribe = args.getSubscriber().subscribe(EventTypes.FinishDecision, (e) => {
    const response = new MoveUserDecisionResponse();
    switch (e.kind) {
      case EventTypes.CancelDecision: {
        if (e.processId !== id) {
          return;
        }
        response.setAction(Action.CANCEL);
        break;
      }
      case EventTypes.FinishDecision: {
        if (e.processId !== id) {
          return;
        }

        switch (e.resultAction.kind) {
          case DecisionAction.Overwrite:
            response.setAction(Action.OVERWRITE);
            break;
          case DecisionAction.Rename:
            response.setAction(Action.RENAME);
            response.setNewName(e.resultAction.newName);
            break;
        }
      }
    }

    lazyResponse(ProcessResult.successed(response.serializeBinary()));
    unsubscribe();
    args.getCommandExecutor().execute(reset, args.getState(), undefined);
  });
};

/**
 * handler for `MoveUserDecisionRequest`
 */
const handleDeleteUserDecision: InnerCommandHandler = (args, id, payload, lazyResponse) => {
  const logger = winston.loggers.get(Loggers.RPC);
  const request = DeleteUserDecisionRequest.deserializeBinary(payload);
  const factory = args.getCommandResolver().resolveBy(descriptors.decisionRequest);
  const reset = args.getCommandResolver().resolveBy(descriptors.decisionReset);
  const item = request.getItem();
  if (!factory || !item || !reset) {
    logger.error(`Invalid path: ${descriptors.filerUpdate.identifier}`);
    lazyResponse(ProcessResult.ignored());
    return;
  }

  logger.info("Start handling event request decision for move");

  args.getCommandExecutor().execute(factory, args.getState(), {
    requiredOp: DecisionRequiredOp.Move,
    fileItem: item,
    processId: id,
  });
  const unsubscribe = args.getSubscriber().subscribe(EventTypes.FinishDecision, (e) => {
    const response = new DeleteUserDecisionResponse();

    switch (e.kind) {
      case EventTypes.CancelDecision: {
        if (e.processId !== id) {
          return;
        }
        response.setConfirmed(false);
        break;
      }
      case EventTypes.FinishDecision: {
        if (e.processId !== id) {
          return;
        }

        response.setConfirmed(false);
        switch (e.resultAction.kind) {
          case DecisionAction.Confirm:
            response.setConfirmed(true);
            break;
          default:
            break;
        }
      }
    }

    lazyResponse(ProcessResult.successed(response.serializeBinary()));
    unsubscribe();
    args.getCommandExecutor().execute(reset, args.getState(), undefined);
  });
};

const handleCandidateUpdated: InnerCommandHandler = (args, _, payload, lazyResponse) => {
  const logger = winston.loggers.get(Loggers.RPC);
  const request = CompletionResultNotificationRequest.deserializeBinary(payload);
  const factory = args.getCommandResolver().resolveBy(descriptors.completerUpdateCandidates);
  if (!factory) {
    logger.error(`Invalid path: ${descriptors.completerUpdateCandidates.identifier}`);
    lazyResponse(ProcessResult.ignored());
    return;
  }

  logger.info("Start handling event request to be notified candidates");

  args.getCommandExecutor().execute(factory, args.getState(), { candidates: request.getCandidatesList() });

  lazyResponse(ProcessResult.successed(new CompletionResultNotificationResponse().serializeBinary()));
};

const handleConfigurationUpdated: InnerCommandHandler = (args, _, payload, lazyResponse) => {
  // TODO: Need to notify partial configuration from server.
  const logger = winston.loggers.get(Loggers.RPC);
  const request = Con.UpdatedNotificationRequest.deserializeBinary(payload);
  const factory = args.getCommandResolver().resolveBy(descriptors.configurationUpdateAll);
  if (!factory) {
    logger.error(`Invalid path: ${descriptors.configurationUpdateAll.identifier}`);
    lazyResponse(ProcessResult.ignored());
    return;
  }

  logger.info("Start handling event configuration_updated");

  args.getCommandExecutor().execute(factory, args.getState(), { configurations: request.getConfigurationsList() });

  lazyResponse(ProcessResult.successed(new Con.UpdatedNotificationResponse().serializeBinary()));
};

/**
 * The notification handler for pushing notification from server
 */
export const notificationHandler = function notificationHandler(
  args: Args
): (id: string, command: number, payload: Uint8Array, lazyResponse: ResponseSender) => void {
  return (id, command, payload, lazyResponse) => {
    const logger = winston.loggers.get(Loggers.RPC);
    switch (command) {
      case Command.FILER_UPDATED:
        {
          const request = UpdatedNotificationRequest.deserializeBinary(payload);
          const factory = args.getCommandResolver().resolveBy(descriptors.filerUpdate);
          if (!factory) {
            logger.error(`Invalid path: ${descriptors.filerUpdate.identifier}`);
            lazyResponse(ProcessResult.ignored());
            return;
          }

          logger.info("Start handling event filer_updated");

          args.getCommandExecutor().execute(factory, args.getState(), { filer: request.getFiler() });

          lazyResponse(ProcessResult.successed(new UpdatedNotificationResponse().serializeBinary()));
        }
        break;
      case Command.FILER_UPDATED_FILE_WINDOW:
        {
          const request = UpdatedFileWindowNotificationRequest.deserializeBinary(payload);
          const factory = args.getCommandResolver().resolveBy(descriptors.filerUpdateFileWindow);
          if (!factory) {
            logger.error(`Invalid path: ${descriptors.filerUpdateFileWindow.identifier}`);
            lazyResponse(ProcessResult.ignored());
            return;
          }

          logger.info("Start handling event filer_updated_file_window");

          const side = request.getSide() === SideMap.LEFT ? Side.Left : Side.Right;
          args.getCommandExecutor().execute(factory, args.getState(), { side, fileWindow: request.getFileWindow() });

          lazyResponse(ProcessResult.successed(new UpdatedFileWindowNotificationResponse().serializeBinary()));
        }
        break;
      case Command.CONFIGURATION_NOTIFY_UPDATED:
        handleConfigurationUpdated(args, id, payload, lazyResponse);
        break;
      case Command.FILER_COPY_INTERACTION:
        handleCopyUserDecision(args, id, payload, lazyResponse);
        break;
      case Command.FILER_MOVE_INTERACTION:
        handleMoveUserDecision(args, id, payload, lazyResponse);
        break;
      case Command.FILER_DELETE_INTERACTION:
        handleDeleteUserDecision(args, id, payload, lazyResponse);
        break;
      case Command.COMPLETER_NOTIFY_COMPLETED:
        handleCandidateUpdated(args, id, payload, lazyResponse);
        break;
      default:
        lazyResponse(ProcessResult.ignored());
        break;
    }
  };
};

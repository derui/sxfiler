import { ContextLike } from "./context";
import * as TaskRequireInteractionUseCase from "./usecases/task/require-interaction";
import * as ReceiveMessageNotificationUseCase from "./usecases/notification/receive-message-notification";
import * as ReceiveProgressNotificationUseCase from "./usecases/notification/receive-progress-notification";
import * as TaskFinishedUseCase from "./usecases/task/finished";
import * as FilerUpdatedUseCase from "./usecases/filer/filer-updated";
import { createSuggestions, SuggestionKind } from "./domains/task-suggestion";
import { FilerOnRPC, encode } from "./codecs/filer";
import { createMessage } from "./domains/message-notification";
import { createProgress } from "./domains/progress-notification";

/**
   Handle common notification that contains message or progress of a server.
 */
export const handleMessageNotification = function handleMessageNotification(context: ContextLike) {
  return (params: any) => {
    const { id, level, body } = params;

    if (!id || !level || !body) {
      throw Error("Invalid parameter format");
    }

    const notification = createMessage({ id, level, message: body });
    context.use(ReceiveMessageNotificationUseCase.createUseCase())({ notification });
  };
};

export const handleProgressNotification = function handleProgressNotification(context: ContextLike) {
  return (params: any) => {
    const { id, body } = params;

    if (!id || !body) {
      throw Error("Invalid parameter format");
    }

    const notification = createProgress(id, body);
    context.use(ReceiveProgressNotificationUseCase.createUseCase())({ notification });
  };
};

/**
   Handle a notification to require interaction of a task
 */
export const handleTaskInteraction = function handleTaskInteraction(context: ContextLike) {
  return (params: { taskId: string; itemName: string; suggestions: SuggestionKind[] }) => {
    const suggestions = createSuggestions(params);
    context.use(TaskRequireInteractionUseCase.createUseCase())({ suggestions });
  };
};

/**
   Handle a notification that contains the task id finished
 */
export const handleTaskFinished = function handleTaskFinished(context: ContextLike) {
  return (params: string) => {
    context.use(TaskFinishedUseCase.createUseCase())(params);
  };
};

/**
   Handle a notification that contains the task id finished
 */
export const handleFilerUpdated = function handleFilerUpdated(context: ContextLike) {
  return (params: FilerOnRPC) => {
    context.use(FilerUpdatedUseCase.createUseCase())({ filer: encode(params) });
  };
};

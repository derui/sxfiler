import { NotificationKind, createMessage, createProgress } from "./domains/notification";
import { ContextLike } from "./context";
import * as TaskRequireInteractionUseCase from "./usecases/task/require-interaction";
import * as TaskFinishedUseCase from "./usecases/task/finished";
import { createSuggestions } from "./domains/task-suggestion";

/**
   Handle common notification that contains message or progress of a server.
 */
export const handleNotification = (params: any) => {
  const { id, level, body } = params;

  if (!id || !level || !body) {
    throw Error("Invalid parameter format");
  }

  switch (body.type) {
    case NotificationKind.Message:
      return createMessage(id, level, body.message);
    case NotificationKind.Progress:
      return createProgress(id, level, body.progress);
    default:
      throw Error(`Unknown notification format: ${params}`);
  }
};

/**
   Handle a notification to require interaction of a task
 */
export const handleTaskInteraction = (context: ContextLike) => (params: any) => {
  if (!params) {
    throw Error("Params should be not null in definition");
  }

  const suggestions = createSuggestions(params);
  context.execute(TaskRequireInteractionUseCase.createUseCase(), { suggestions });
};

/**
   Handle a notification that contains the task id finished
 */
export const handleTaskFinished = (context: ContextLike) => (params: any) => {
  if (!params) {
    throw Error("Params should be not null in definition");
  }

  context.execute(TaskFinishedUseCase.createUseCase(), params);
};

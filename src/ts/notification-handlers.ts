import { ContextLike } from "./context";
import * as TaskRequireInteractionUseCase from "./usecases/task/require-interaction";
import * as ReceiveMessageNotificationUseCase from "./usecases/notification/receive-message-notification";
import * as ReceiveProgressNotificationUseCase from "./usecases/notification/receive-progress-notification";
import * as TaskFinishedUseCase from "./usecases/task/finished";
import * as FilerUpdatedUseCase from "./usecases/filer/filer-updated";
import { createSuggestions, Suggestion } from "./domains/task-suggestion";
import { FilerOnRPC, encode } from "./codecs/filer";
import { createMessage } from "./domains/message-notification";
import { createProgress } from "./domains/progress-notification";

/**
   Handle common notification that contains message or progress of a server.
 */
export const handleMessageNotification = (context: ContextLike) => (params: any) => {
  const { id, level, body } = params;

  if (!id || !level || !body) {
    throw Error("Invalid parameter format");
  }

  const notification = createMessage({ id, level, message: body });
  context.use(ReceiveMessageNotificationUseCase.createUseCase()).execute({ notification });
};

export const handleProgressNotification = (context: ContextLike) => (params: any) => {
  const { id, body } = params;

  if (!id || !body) {
    throw Error("Invalid parameter format");
  }

  const notification = createProgress(id, body);
  context.use(ReceiveProgressNotificationUseCase.createUseCase()).execute({ notification });
};

/**
   Handle a notification to require interaction of a task
 */
export const handleTaskInteraction = (context: ContextLike) => (params: {
  taskId: string;
  nodeName: string;
  suggestions: Suggestion[];
}) => {
  console.log(params);
  const suggestions = createSuggestions(params);
  context.use(TaskRequireInteractionUseCase.createUseCase()).execute({ suggestions });
};

/**
   Handle a notification that contains the task id finished
 */
export const handleTaskFinished = (context: ContextLike) => (params: string) => {
  context.use(TaskFinishedUseCase.createUseCase()).execute(params);
};

/**
   Handle a notification that contains the task id finished
 */
export const handleFilerUpdated = (context: ContextLike) => (params: FilerOnRPC) => {
  context.use(FilerUpdatedUseCase.createUseCase()).execute({ filer: encode(params) });
};

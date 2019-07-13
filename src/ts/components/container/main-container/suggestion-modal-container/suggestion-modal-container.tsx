import * as React from "react";
import { State } from "@/states/task-interaction";
import ModalRootContext from "@/modal-root";
import * as SuggestionModal from "@/components/project/suggestion-modal/suggestion-modal";
import { ReplyPayload } from "@/domains/task-reply";
import { ContextLike } from "@/context";
import * as UseCase from "@/usecases/task/update-reply-payload";
import LocatorContext from "@/locator";

export type Props = {
  state: State;
};

export type ElementType = React.ReactElement<Props, React.FC<Props>>;

const handleReply = (context: ContextLike) => (reply: ReplyPayload) => {
  context.use(UseCase.createUseCase()).execute(reply);
};

// Stateless container to render filer
export const Component: React.FC<Props> = ({ state }): ElementType | null => {
  return (
    <LocatorContext.Consumer>
      {locator => {
        const context = locator.context;
        if (!context) {
          return null;
        }

        return (
          <ModalRootContext.Consumer>
            {({ element }) =>
              element && (
                <SuggestionModal.Component
                  dialogRoot={element}
                  opened={state.operating}
                  container={{
                    focusedReply: state.currentReplyIndex || 0,
                    replies: state.replies || [],
                    onReply: handleReply(context),
                  }}
                />
              )
            }
          </ModalRootContext.Consumer>
        );
      }}
    </LocatorContext.Consumer>
  );
};

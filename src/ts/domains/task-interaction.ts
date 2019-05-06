// type of interaction
export enum InteractionKind {
  YesNo = "yes-no",
  String = "string",
  Int = "int",
}

export type InteractionObject = {
  readonly id: string;
  readonly taskId: string;
  readonly acceptInteractions: InteractionKind[];
};

export type Interaction = InteractionObject;

/**
   The factory function to create interaction from argument
 */
export const createInteraction = (args: {
  id: string;
  taskId: string;
  acceptInteractions: InteractionKind[];
}): Interaction => {
  return {
    ...args,
  };
};

import { CommandDescriptor, CommandFactory } from "./type";

/**
 * Command spec to register a command
 */
export type Spec<T> = {
  factory: CommandFactory<T>;
  descriptor: CommandDescriptor<T>;
};

/**
 * The interface for command resolver
 */
export type Type = {
  register<T>(spec: Spec<T>): void;

  /**
   * resolve by command descriptor
   */
  resolveBy<T>(descriptor: CommandDescriptor<T>): CommandFactory<T> | undefined;

  /**
   * resolve by command identifier. This function can not restore type in descriptor.
   */
  resolveById(id: string): CommandFactory<any> | undefined;
};

export const create = function create(): Type {
  const _commandSpecs = new Map<string, CommandFactory<any>>();

  return {
    register<T>(spec: Spec<T>) {
      _commandSpecs.set(spec.descriptor.identifier, spec.factory);
    },

    resolveBy<T>(descriptor: CommandDescriptor<T>): CommandFactory<T> | undefined {
      // erase payload type to ignore compile error.
      const factory = _commandSpecs.get(descriptor.identifier);

      if (!factory) {
        return undefined;
      }

      return factory as CommandFactory<T>;
    },

    resolveById(id: string): CommandFactory<any> | undefined {
      // erase payload type to ignore compile error.
      const factory = _commandSpecs.get(id);

      if (!factory) {
        return undefined;
      }

      return factory as CommandFactory<any>;
    },
  };
};

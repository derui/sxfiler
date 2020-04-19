import { notificationHandler } from "./notification-handler";
import { Command } from "@/generated/service_pb";
import {
  UpdatedNotificationRequest,
  Filer,
  UpdatedNotificationResponse,
  InitializeRequest,
} from "@/generated/filer_pb";
import { CommandDescriptor, CommandFactory } from "@/commands/type";
import * as CR from "@/commands/command-resolver";
import * as filerUpdate from "@/commands/internal/filer/update";
import { ProcessResult } from "@/libs/websocket-rpc/server";

describe("RPC", () => {
  describe("Notification Handler", () => {
    test("do not handle some commands that is not notification", async () => {
      let executor = {
        execute: jest.fn(),
      };
      let resolver = {
        register<T>(_: CR.Spec<T>) {},
        resolveBy<T>(_: CommandDescriptor<T>): CommandFactory<T> | undefined {
          return jest.fn();
        },
        resolveById(_: string): CommandFactory<any> | undefined {
          return jest.fn();
        },
      };

      const handler = notificationHandler({
        getState() {
          return {} as any;
        },
        getCommandResolver() {
          return resolver;
        },
        getCommandExecutor() {
          return executor;
        },
        getSubscriber() {
          return jest.fn() as any;
        },
      });

      const request = new InitializeRequest();
      const promise = new Promise((resolve) => {
        const response = (v: any) =>
          new Promise<void>((r) => {
            resolve(v);
            r();
          });
        handler("id", Command.FILER_INITIALIZE, request.serializeBinary(), response);
      });
      const res = await promise;

      expect(res).toEqual(ProcessResult.ignored());
      expect(executor.execute).not.toBeCalled();
    });

    test("should handle notification that filer is updated", async () => {
      const filer = new Filer();
      let executor = {
        async execute(resolver: any, _: any, payload: any) {
          expect(resolver).toBe(filerUpdate.createCommand);
          expect(payload).toEqual({ filer });
        },
      };

      let resolver: CR.Type = {
        register<T>(_: CR.Spec<T>) {},
        resolveBy<T>(_: CommandDescriptor<T>): CommandFactory<T> | undefined {
          return filerUpdate.createCommand;
        },
        resolveById(_: string): CommandFactory<any> | undefined {
          return jest.fn();
        },
      };

      const handler = notificationHandler({
        getState() {
          return {} as any;
        },
        getCommandResolver() {
          return resolver;
        },
        getCommandExecutor() {
          return executor;
        },
        getSubscriber() {
          return jest.fn() as any;
        },
      });

      const request = new UpdatedNotificationRequest();
      request.setFiler(new Filer());

      const promise = new Promise((resolve) => {
        const response = (v: any) =>
          new Promise<void>((r) => {
            resolve(v);
            r();
          });
        handler("id", Command.FILER_UPDATED, request.serializeBinary(), response);
      });
      const res = await promise;

      expect(res).not.toBeUndefined();
      expect(res).toEqual(ProcessResult.successed(new UpdatedNotificationResponse().serializeBinary()));
    });
  });
});

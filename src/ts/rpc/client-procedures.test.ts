import { Filer } from "./client-procedures";
import {
  InitializeRequest,
  InitializeResponse,
  ReloadAllResponse,
  ReloadAllRequest,
  MoveLocationRequest,
  Side,
  MoveLocationResponse,
} from "@/generated/filer_pb";
import { Request, Response, Status, Command } from "@/generated/service_pb";
import { also } from "@/libs/fn";

describe("RPC", () => {
  describe("Client Procedures", () => {
    describe("Filer", () => {
      test("call procedure to initialize", async () => {
        const client = {
          async send(request: Request) {
            const r = InitializeRequest.deserializeBinary(request.getPayload_asU8());

            expect(request.getCommand()).toEqual(Command.FILER_INITIALIZE);
            expect(r.getLeftLocation()).toEqual("/left");
            expect(r.getRightLocation()).toEqual("/right");

            return also(new Response(), (v) => {
              v.setId(request.getId());
              v.setStatus(Status.SUCCESS);
              v.setPayload(new InitializeResponse().serializeBinary());
            });
          },
          destroy: jest.fn(),
        };

        const request = new InitializeRequest();
        request.setLeftLocation("/left");
        request.setRightLocation("/right");

        const res = await Filer.initialize(client, request);
        expect(res).toEqual(new InitializeResponse());
      });

      test("call procedure to reload all", async () => {
        const client = {
          async send(request: Request) {
            expect(request.getCommand()).toEqual(Command.FILER_RELOAD_ALL);

            return also(new Response(), (v) => {
              v.setId(request.getId());
              v.setStatus(Status.SUCCESS);
              v.setPayload(new InitializeResponse().serializeBinary());
            });
          },
          destroy: jest.fn(),
        };

        const request = new ReloadAllRequest();

        const res = await Filer.reloadAll(client, request);
        expect(res).toEqual(new ReloadAllResponse());
      });

      test("call procedure to move location", async () => {
        const client = {
          async send(request: Request) {
            const p = MoveLocationRequest.deserializeBinary(request.getPayload_asU8());
            expect(request.getCommand()).toEqual(Command.FILER_MOVE_LOCATION);
            expect(p.getSide()).toEqual(Side.LEFT);
            expect(p.getLocation()).toEqual("/others");

            return also(new Response(), (v) => {
              v.setId(request.getId());
              v.setStatus(Status.SUCCESS);
              v.setPayload(new MoveLocationResponse().serializeBinary());
            });
          },
          destroy: jest.fn(),
        };

        const request = new MoveLocationRequest();
        request.setSide(Side.LEFT);
        request.setLocation("/others");

        const res = await Filer.moveLocation(client, request);
        expect(res).toEqual(new MoveLocationResponse());
      });
    });
  });
});

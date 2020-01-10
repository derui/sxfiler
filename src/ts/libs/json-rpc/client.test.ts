import { ClientImpl } from "./client";
import { Request, Response } from "./type";

describe("JSON-RPC library", () => {
  describe("Client", () => {
    describe("Invoke method", () => {
      it("call parameter and result transformer", done => {
        const idGenerator = jest.fn(() => "id");
        const requester = {
          call: (req: Request) => Promise.resolve({ id: req.id, jsonrpc: "2.0" } as Response),
          notify: (req: Request) => {},
        };
        const client = new ClientImpl<"method">(requester, idGenerator);

        client.call(
          {
            method: "method",
            parametersTransformer() {
              return { toJSON: jest.fn() };
            },
            resultTransformer: (ret, err) => {
              expect(ret).toBeUndefined();
              expect(err).toBeUndefined();
              done();
              return [];
            },
          },
          {}
        );
      });
    });

    describe("Notify method", () => {
      it("call parameter transformer", () => {
        const idGenerator = jest.fn(() => "id");
        const requester = {
          call: (req: Request) => Promise.resolve({ id: req.id, jsonrpc: "2.0" } as Response),
          notify: (req: Request) => {
            expect(req.id).toBeUndefined();
            expect(req.params).toEqual(10000);
          },
        };
        const client = new ClientImpl<"method">(requester, idGenerator);

        client.notify(
          {
            method: "method",
            parametersTransformer(req: number) {
              expect(req).toEqual(100);
              return {
                toJSON() {
                  return req * 100;
                },
              };
            },
          },
          100
        );
      });
    });
  });
});

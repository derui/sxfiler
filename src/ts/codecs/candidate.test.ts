import * as E from "./candidate";
import { Candidate, Item } from "@/generated/completion_pb";

describe("Object Codecs", () => {
  describe("Candidate", () => {
    it("convert JSON representation to frontend domain", () => {
      const item = new Item({
        id: "id",
        value: "foobar",
      });
      const candidate = new Candidate({
        length: 10,
        start: 1,
        value: item,
      });

      const obj = E.encode(candidate);

      expect(obj).toEqual({
        id: "id",
        value: "foobar",
        start: 1,
        length: 6,
      });
    });
  });
});

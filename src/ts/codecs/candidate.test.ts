import * as E from "./candidate";
import { Candidate, Item } from "@/generated/completion_pb";

describe("Object Codecs", () => {
  describe("Candidate", () => {
    it("convert JSON representation to frontend domain", () => {
      const candidate = new Candidate();
      const item = new Item();
      item.setId("id");
      item.setValue("foobar");
      candidate.setLength(10);
      candidate.setStart(1);
      candidate.setValue(item);

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

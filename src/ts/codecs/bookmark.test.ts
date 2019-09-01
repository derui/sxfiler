import * as E from "./candidate";

describe("Object Codecs", () => {
  describe("Candidate", () => {
    it("convert JSON representation to frontend domain", () => {
      const obj = E.encode({
        length: 10,
        start: 1,
        value: {
          id: "id",
          value: "foobar",
        },
      });

      expect(obj).toEqual({
        id: "id",
        value: "foobar",
        start: 1,
        length: 6,
      });
    });
  });
});

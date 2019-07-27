import { createCandidate, splitByMatching } from "./candidate";

describe("Domain", () => {
  describe("Completion Candidate", () => {
    it("can create candidate from argument", () => {
      const obj = {
        id: "id",
        value: "value",
        start: 0,
        length: 0,
      };
      const data = createCandidate(obj);

      expect(data).toEqual(obj);
    });

    it("fix start position when was greater than length of the value", () => {
      const obj = createCandidate({
        id: "id",
        value: "v",
        start: 1,
        length: 0,
      });

      expect(obj.start).toEqual(0);
    });

    it("fix length when was greater than length of the value", () => {
      const obj = createCandidate({
        id: "id",
        value: "v",
        start: 0,
        length: 2,
      });

      expect(obj.length).toEqual(1);
    });

    describe("Split value from matching inforation", () => {
      it("return value when no any matched", () => {
        const obj = createCandidate({
          id: "id",
          value: "v",
          start: 0,
          length: 0,
        });

        expect(splitByMatching(obj)).toEqual(["", "", "v"]);
      });

      it("return value when has matched", () => {
        const obj = createCandidate({
          id: "id",
          value: "abcde",
          start: 2,
          length: 2,
        });

        expect(splitByMatching(obj)).toEqual(["ab", "cd", "e"]);
      });

      it("return value when exactly matched", () => {
        const obj = createCandidate({
          id: "id",
          value: "abcde",
          start: 0,
          length: 5,
        });

        expect(splitByMatching(obj)).toEqual(["", "abcde", ""]);
      });

      it("return value matched to last of it", () => {
        const obj = createCandidate({
          id: "id",
          value: "abcde",
          start: 3,
          length: 2,
        });

        expect(splitByMatching(obj)).toEqual(["abc", "de", ""]);
      });
    });
  });
});

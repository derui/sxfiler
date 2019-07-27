import * as E from "./location-history";

describe("Encode", () => {
  describe("Location History", () => {
    it("should encode RPC to frontend", () => {
      const obj = E.encode({ records: [], maxRecordNumber: 100 });

      expect(obj).toEqual({ records: [], maxRecordNumber: 100 });
    });

    it("should encode record object to frontend", () => {
      const obj = E.encode({
        records: [
          {
            location: "foo",
            timestamp: "1234567890",
          },
        ],
        maxRecordNumber: 100,
      });

      expect(obj).toEqual({
        records: [
          {
            location: "foo",
            timestamp: new Date(1234567890),
          },
        ],
        maxRecordNumber: 100,
      });
    });
  });
});

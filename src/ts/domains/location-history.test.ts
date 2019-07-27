import { createLocationHistory } from "./location-history";

describe("Domains", () => {
  describe("LocationHistory", () => {
    it("should create location history", () => {
      const obj = createLocationHistory({
        records: [],
        maxRecordNumber: 100,
      });

      expect(obj).toEqual({
        records: [],
        maxRecordNumber: 100,
      });
    });
  });
});

import * as E from "./location-history";
import { LocationHistory, LocationRecord } from "@/generated/filer_pb";

function makeRecord(location: string, timestamp: string) {
  const ret = new LocationRecord({
    location,
    timestamp,
  });
  return ret;
}

describe("Encode", () => {
  describe("Location History", () => {
    it("should encode RPC to frontend", () => {
      const req = new LocationHistory();
      const obj = E.encode(req);

      expect(obj).toEqual({ records: [], maxRecordNumber: 0 });
    });

    it("should encode record object to frontend", () => {
      const req = new LocationHistory({
        records: [makeRecord("foo", "1234567890")],
        maxRecordNumber: 100,
      });
      const obj = E.encode(req);

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

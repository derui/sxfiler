import * as E from "./location-history";
import { LocationHistory, LocationRecord } from "@/generated/filer_pb";

function makeRecord(location: string, timestamp: string) {
  const ret = new LocationRecord();
  ret.setLocation(location);
  ret.setTimestamp(timestamp);
  return ret;
}

describe("Encode", () => {
  describe("Location History", () => {
    it("should encode RPC to frontend", () => {
      const req = new LocationHistory();
      const obj = E.encode(req);

      expect(obj).toEqual({ records: [], maxRecordNumber: 100 });
    });

    it("should encode record object to frontend", () => {
      const req = new LocationHistory();
      req.setRecordsList([makeRecord("foo", "1234567890")]);
      req.setMaxrecordnumber(100);
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

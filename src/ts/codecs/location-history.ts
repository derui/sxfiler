import { LocationHistory as Domain, createLocationHistory } from "@/domains/location-history";
import { LocationRecord, LocationHistory } from "@/generated/filer_pb";

const encodeLocationRecord = function encodeLocationRecord(record: LocationRecord) {
  return { location: record.getLocation(), timestamp: new Date(Number(record.getTimestamp())) };
};

/**
   encode location history object from RPC to frontend domain.

   @param obj JSON representation for location history
   @return LocationHistory object
 */
export const encode = function encode(obj: LocationHistory): Domain {
  return createLocationHistory({
    records: obj.getRecordsList().map(encodeLocationRecord),
    maxRecordNumber: obj.getMaxrecordnumber(),
  });
};

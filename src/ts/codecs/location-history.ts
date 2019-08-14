import { LocationHistory, createLocationHistory } from "@/domains/location-history";

// define codec that is between filer domain and RPC

type LocationRecordOnRPC = {
  location: string;
  timestamp: string;
};

export type LocationHistoryOnRPC = {
  records: LocationRecordOnRPC[];
  maxRecordNumber: number;
};

const encodeLocationRecord = function encodeLocationRecord(record: LocationRecordOnRPC) {
  return { location: record.location, timestamp: new Date(Number(record.timestamp)) };
};

/**
   encode location history object from RPC to frontend domain.

   @param obj JSON representation for location history
   @return LocationHistory object
 */
export const encode = function encode(obj: LocationHistoryOnRPC): LocationHistory {
  return createLocationHistory({
    records: obj.records.map(encodeLocationRecord),
    maxRecordNumber: obj.maxRecordNumber,
  });
};

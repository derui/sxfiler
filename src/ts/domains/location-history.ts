// histories of location moved filer before

export type LocationRecord = {
  readonly location: string;
  readonly timestamp: Date;
};

export type LocationHistory = {
  readonly records: LocationRecord[];
  readonly maxRecordNumber: number;
};

// creare new LocationHistory
export const createLocationHistory = function createLocationHistory({
  records,
  maxRecordNumber,
}: LocationHistory): LocationHistory {
  return {
    records,
    maxRecordNumber,
  };
};

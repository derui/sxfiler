// histories of location moved filer before

export type LocationRecord = {
  readonly location: string;
  readonly timestamp: Date;
};

export type LocationHistoryObject = {
  readonly records: LocationRecord[];
  readonly maxRecordNumber: number;
};

export type LocationHistory = LocationHistoryObject & {
  plain(): LocationHistoryObject;
};

// return plain object
function plain(this: LocationHistory) {
  return { records: this.records, maxRecordNumber: this.maxRecordNumber };
}

// creare new LocationHistory
export const createLocationHistory = ({ records, maxRecordNumber }: LocationHistoryObject): LocationHistory => {
  return {
    records,
    maxRecordNumber,
    plain,
  };
};

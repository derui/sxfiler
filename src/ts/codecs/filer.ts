import { Filer as Domain, createFiler } from "@/domains/filer";
import { encode as encodeFileItem } from "./file-item";
import { encode as encodeLocationHistory } from "./location-history";
import { Filer } from "@/generated/filer_pb";
import { createLocationHistory } from "@/domains/location-history";

/**
   encode filer object from RPC to frontend domain.

   @param filer JSON representation for fller
   @return Filer object
 */
export const encode = function encode(obj: Filer): Domain {
  const markedItems = obj.getMarkeditemsList();
  return createFiler({
    id: obj.getId(),
    name: obj.getName(),
    location: obj.getFilelist()?.getLocation() || "",
    items:
      obj
        .getFilelist()
        ?.getItemsList()
        ?.map(v => encodeFileItem(v, markedItems.includes(v.getId()))) || [],
    currentCursorIndex: 0,
    history:
      encodeLocationHistory(obj.getHistory()) ||
      createLocationHistory({
        records: [],
        maxRecordNumber: 0,
      }),
  });
};

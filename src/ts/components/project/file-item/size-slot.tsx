import { h } from "preact";

enum SizeUnit {
  Byte = "byte",
  KByte = "kbyte",
  MByte = "mbyte",
  GByte = "gbyte",
  TByte = "tbyte",
  Unknown = "unknown",
}

type SizeData = {
  sizeUnit: SizeUnit;
  size: bigint;
  alignedSize: bigint;
};

const nextUnit = function nextUnit(sizeUnit: SizeUnit): SizeUnit | null {
  switch (sizeUnit) {
    case SizeUnit.Byte:
      return SizeUnit.KByte;
    case SizeUnit.KByte:
      return SizeUnit.MByte;
    case SizeUnit.MByte:
      return SizeUnit.GByte;
    case SizeUnit.GByte:
      return SizeUnit.TByte;
    case SizeUnit.TByte:
      return SizeUnit.Unknown;
    default:
      return null;
  }
};

const sizeUnitToString = function sizeUnitToString(sizeUnit: SizeUnit): string {
  switch (sizeUnit) {
    case SizeUnit.Byte:
      return "B";
    case SizeUnit.KByte:
      return "K";
    case SizeUnit.MByte:
      return "M";
    case SizeUnit.GByte:
      return "G";
    case SizeUnit.TByte:
      return "T";
    default:
      return "-";
  }
};

class Size {
  private data: SizeData;

  constructor(size: bigint) {
    this.data = this.toData(size);
  }
  /**
   * convert to String
   */
  public toString(): string {
    const unit = sizeUnitToString(this.data.sizeUnit);
    const fixed = this.data.alignedSize.toString();
    const padded = fixed.padStart(5, " ");
    return `${padded}${unit}`;
  }

  /**
   * parse size string to data structure
   * @param size string representation of size
   * @return size data
   */
  private toData(size: bigint): SizeData {
    const kb = BigInt(1024);
    const calcUnit = (fileSize: bigint, current: SizeUnit, decimal: bigint): { current: SizeUnit; size: bigint } => {
      if (BigInt(0) <= fileSize && fileSize < kb) {
        return { current, size: fileSize };
      }

      const unit = nextUnit(current);
      if (unit == null) {
        return { current, size: fileSize + decimal };
      } else {
        return calcUnit(fileSize / kb, unit, (fileSize % kb) / kb);
      }
    };
    const data = calcUnit(size, SizeUnit.Byte, BigInt(0));

    return {
      sizeUnit: data.current,
      alignedSize: data.size,
      size,
    };
  }
}

export type Props = {
  size: bigint;
};

export const Component: preact.FunctionComponent<Props> = (prop) => {
  const size = new Size(prop.size);

  return (
    <span class="file-item__item-size" data-testid="fileItem-sizeSlot">
      {size.toString()}
    </span>
  );
};

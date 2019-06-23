import bigInt from "big-integer";
import * as React from "react";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const styles = require("./file-item.module.scss");

enum SizeUnit {
  Byte = "byte",
  KByte = "kbyte",
  MByte = "mbyte",
  GByte = "gbyte",
  TByte = "tbyte",
  Unknown = "unknown",
}

interface SizeData {
  sizeUnit: SizeUnit;
  size: bigInt.BigInteger;
  alignedSize: number;
}

function nextUnit(sizeUnit: SizeUnit): SizeUnit | null {
  switch (sizeUnit) {
    case "byte":
      return SizeUnit.KByte;
    case "kbyte":
      return SizeUnit.MByte;
    case "mbyte":
      return SizeUnit.GByte;
    case "gbyte":
      return SizeUnit.TByte;
    case "tbyte":
      return SizeUnit.Unknown;
    default:
      return null;
  }
}

function sizeUnitToString(sizeUnit: SizeUnit): string {
  switch (sizeUnit) {
    case "byte":
      return "B";
    case "kbyte":
      return "K";
    case "mbyte":
      return "M";
    case "gbyte":
      return "G";
    case "tbyte":
      return "T";
    default:
      return "-";
  }
}

class Size {
  private data: SizeData;

  constructor(size: bigInt.BigInteger) {
    this.data = this.toData(size);
  }
  /**
   * convert to String
   */
  public toString(): string {
    const unit = sizeUnitToString(this.data.sizeUnit);
    const fixed = this.data.alignedSize.toFixed(1);
    const padded = fixed.padStart(5, " ");
    return `${padded}${unit}`;
  }

  /**
   * parse size string to data structure
   * @param size string representation of size
   * @return size data
   */
  private toData(size: bigInt.BigInteger): SizeData {
    function calcUnit(
      fileSize: bigInt.BigInteger,
      current: SizeUnit,
      decimal: number
    ): { current: SizeUnit; size: number } {
      if (bigInt.zero.leq(fileSize) && fileSize.lt(bigInt(1024))) {
        return { current, size: fileSize.toJSNumber() };
      }

      const unit = nextUnit(current);
      if (unit == null) {
        return { current, size: fileSize.toJSNumber() + decimal };
      } else {
        return calcUnit(fileSize.divide(bigInt(1024)), unit, fileSize.mod(bigInt(1024)).toJSNumber() / 1024);
      }
    }
    const parsedSize = bigInt(size);
    const data = calcUnit(parsedSize, SizeUnit.Byte, 0);

    return {
      sizeUnit: data.current,
      alignedSize: data.size,
      size: parsedSize,
    };
  }
}

interface Prop {
  size: bigInt.BigInteger;
}

const NodeSize: React.FC<Prop> = prop => {
  const size = new Size(prop.size);

  return <pre className={styles.size}>{size.toString()}</pre>;
};

export default NodeSize;

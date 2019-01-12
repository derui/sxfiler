import * as React from "react";
import bigInt from "big-integer";

enum SizeUnit {
  Byte = "byte",
  KByte = "kbyte",
  MByte = 'mbyte',
  GByte = 'gbyte',
  TByte = 'tbyte',
  Unknown = 'unknown',
}

type SizeData = {
  sizeUnit: SizeUnit;
  size: bigInt.BigInteger;
  alignedSize: number;
}

function nextUnit(sizeUnit: SizeUnit) : SizeUnit | null {
  switch (sizeUnit){
    case "byte": return SizeUnit.KByte;
    case "kbyte": return SizeUnit.MByte;
    case "mbyte": return SizeUnit.GByte;
    case "gbyte": return SizeUnit.TByte;
    case "tbyte": return SizeUnit.Unknown;
    default: return null;
  }
}

function sizeUnitToString(sizeUnit: SizeUnit) : string {
  switch (sizeUnit) {
    case "byte": return "B";
    case "kbyte": return "K";
    case "mbyte": return "M";
    case "gbyte": return "G";
    case "tbyte": return "T";
    default: return "-";
  }
}

class Size {
  private data: SizeData;

  constructor(size: string) {
    this.data = this.toData(size);
  };

  /**
   * parse size string to data structure
   * @param size string representation of size
   * @return size data
   */
  private toData(size: string): SizeData {
    function calcUnit(size: bigInt.BigInteger, current: SizeUnit, decimal: number) : {current: SizeUnit, size: number} {
      if (bigInt.zero.leq(size) && size.lt(bigInt(1024))) {
        return {current, size: size.toJSNumber()};
      }

      const unit = nextUnit(current);
      if (unit == null) {
        return {current, size: size.toJSNumber() + decimal};
      } else {
        return calcUnit(size.divide(bigInt(1024)), unit, size.mod(bigInt(1024)).toJSNumber() / 1024)
      }
    }
    const parsedSize = bigInt(size);
    const data = calcUnit(parsedSize, SizeUnit.Byte, 0);

    return {
      sizeUnit: data.current,
      alignedSize: data.size,
      size: parsedSize
    };
  }

  /**
   * convert to String
   */
  toString(): string {
    const unit = sizeUnitToString(this.data.sizeUnit);
    const fixed = this.data.alignedSize.toFixed(1);
    return `${fixed}${unit}`;
  }

}

interface Prop {
  size: string;
}

const FileItemSize : React.FC<Prop> = prop => {
  const size = new Size(prop.size);

  return (<span className="fp-FileItem_FileSize">{size.toString()}</span>)
}

export default FileItemSize;

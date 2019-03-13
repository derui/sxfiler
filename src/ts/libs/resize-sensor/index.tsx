import * as React from "react";

const style: React.CSSProperties = {
  position: "absolute",
  left: -10,
  top: -10,
  right: 0,
  bottom: 0,
  overflow: "hidden",
  zIndex: -1,
  visibility: "hidden",
};

const expandChildStyle: React.CSSProperties = {
  position: "absolute",
  left: 0,
  top: 0,
};

const shrinkChildStyle: React.CSSProperties = {
  position: "absolute",
  left: 0,
  top: 0,
  transition: "0s",
  width: "200%",
  height: "200%",
};

type ScrollHandler = (event: React.UIEvent) => void;

function makeShrink(onScroll: ScrollHandler, ref: React.RefObject<HTMLDivElement>) {
  return (
    <div key="shrink" onScroll={onScroll} style={style} ref={ref}>
      <div key="shrinkChild" style={shrinkChildStyle} />
    </div>
  );
}

function makeExpand(
  onScroll: ScrollHandler,
  ref: React.RefObject<HTMLDivElement>,
  childRef: React.RefObject<HTMLDivElement>
) {
  return (
    <div key="expand" onScroll={onScroll} style={style} ref={ref}>
      <div key="expandChild" style={expandChildStyle} ref={childRef} />
    </div>
  );
}

type Size = {
  width: number;
  height: number;
};

export interface Props {
  onResize: (size: Size) => void;
  getParentSize: () => Size;
}

export default class ResizeSensor extends React.Component<Props> {
  private shrinkRef: React.RefObject<HTMLDivElement> = React.createRef();
  private expandRef: React.RefObject<HTMLDivElement> = React.createRef();
  private expandChildRef: React.RefObject<HTMLDivElement> = React.createRef();

  private rafId: number | null = null;
  private dirty = true;
  private lastSize: Size = { width: 0, height: 0 };
  private currentSize: Size = { width: 0, height: 0 };

  private handleResize = () => {
    this.rafId = null;

    if (this.dirty) {
      this.lastSize = this.currentSize;
      this.props.onResize(this.currentSize);
    }
  };

  private handleScroll = () => {
    const size = this.props.getParentSize();
    this.dirty = this.lastSize.width !== size.width || this.lastSize.height !== size.height;
    this.currentSize = size;

    if (this.dirty && !this.rafId) {
      let cb = this.handleResize;
      let id = window.requestAnimationFrame(cb);

      if (id) {
        this.rafId = id;
      }

      this.resetSensorElement();
    }
  };

  private resetSensorElement = () => {
    const shrink = this.shrinkRef.current;
    const expand = this.expandRef.current;
    const expandChild = this.expandChildRef.current;

    if (shrink && expand && expandChild) {
      shrink.scrollLeft = 1000000;
      shrink.scrollTop = 1000000;
      expand.scrollLeft = 1000000;
      expand.scrollTop = 1000000;

      expandChild.style.width = "1000000px";
      expandChild.style.height = "1000000px";
    }
  };

  public componentDidMount() {
    this.resetSensorElement();
  }

  public render() {
    return (
      <div style={style}>
        {makeExpand(this.handleScroll, this.expandRef, this.expandChildRef)}
        {makeShrink(this.handleScroll, this.shrinkRef)}
      </div>
    );
  }
}

import * as React from "react";

interface Prop {
  start: number;
  windowSize: number;
}

const ScrollBar : React.FC<Prop> = ({start, windowSize}) => {
  const top = `${start * 100.0}%`;
  const height = `${windowSize * 100.0}%`;

  return (
    <div className="fp-ScrollBar">
      <span className="fp-ScrollBar_VisibleWindow"/>
    </div>
  );
};

export default ScrollBar;

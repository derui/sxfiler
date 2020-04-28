import { h } from "preact";
import * as FileList from "@/components/project/file-list";
import * as F from "@/modules/filer";
import * as FilerReducer from "@/modules/filer/reducer";
import * as FilerPb from "@/generated/filer_pb";
import { Type } from "@/types/natural-number";
import {
  leftSideCursorPositionSelector,
  leftSideFileListSelector,
  rightSideFileListSelector,
  rightSideCursorPositionSelector,
} from "@/modules/filer/selectors";

export type Props = {
  state: F.State;
};

/* create filer from state and key */
const createFiler = (
  side: FilerReducer.Side,
  currentCursorIndex: Type,
  currentSide: FilerReducer.Side,
  fileList?: FilerPb.FileList
): FileList.ElementType | null => {
  const focused = side === currentSide;

  if (!fileList) {
    return null;
  }

  return (
    <FileList.Component
      key={side}
      items={fileList.toObject().itemsList}
      cursor={currentCursorIndex.value}
      location={fileList.toObject().location}
      focused={focused}
      bookmarks={[]}
    />
  );
};

// Stateless container to render filer
export const Component: preact.FunctionComponent<Props> = ({ state }): ElementType | null => {
  // can not render anything if filer is not initialized
  const filer = state.filer;
  if (!filer) {
    return null;
  }

  const leftFiler = createFiler(
    FilerReducer.Side.Left,
    leftSideCursorPositionSelector(state),
    state.currentSide,
    leftSideFileListSelector(state)
  );
  const rightFiler = createFiler(
    FilerReducer.Side.Right,
    rightSideCursorPositionSelector(state),
    state.currentSide,
    rightSideFileListSelector(state)
  );

  return (
    <div class="filer-container__root" data-testid="filerContainer">
      {leftFiler}
      <div class="filer-container__separator" data-testid="filerContainer-separator" />
      {rightFiler}
    </div>
  );
};

export type ElementType = ReturnType<typeof Component>;

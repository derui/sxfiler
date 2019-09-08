import * as React from "react";
import { styled } from "@/components/theme";
import { Filer } from "@/domains/filer";
import { Side, State } from "@/states/file-list";
import * as FileList from "@/components/project/file-list";
import * as Element from "@/components/ui/element";
import { Bookmark } from "@/domains/bookmark";

export type Props = {
  state: State;
};

const Root = styled(Element.Component)`
  display: grid;
  grid-template-rows: auto;
  grid-template-columns: 0.5fr auto 0.5fr;

  color: ${props => props.theme.colors.base1};
  overflow: hidden;
  background-color: ${props => props.theme.colors.base03};

  font-size: 1rem;
`;

const Separator = styled.div`
  width: 2px;
  background-color: ${props => props.theme.colors.base00};
  margin: 0 ${props => props.theme.spaces.nano};
`;

/* create filer from state and key */
const createFiler = function createFiler(
  key: string,
  currentSide: Side,
  bookmarks: { [key: string]: Bookmark },
  filer?: Filer
): FileList.ElementType | null {
  const focused = key === currentSide;

  if (!filer) {
    return null;
  }

  return (
    <FileList.Component
      key={key}
      items={filer.items}
      cursor={filer.currentCursorIndex}
      location={filer.location}
      focused={focused}
      bookmarks={Object.values(bookmarks)}
    />
  );
};

export type ElementType = React.ReactElement<Props, React.FC<Props>>;

// Stateless container to render filer
export const Component: React.FC<Props> = ({ state }): ElementType | null => {
  // can not render anything if filer is not initialized
  if (!state.left || !state.right) {
    return null;
  }

  const leftFiler = createFiler(Side.Left, state.currentSide, state.bookmarks, state.left);
  const rightFiler = createFiler(Side.Right, state.currentSide, state.bookmarks, state.right);

  return (
    <Root>
      {leftFiler}
      <Separator />
      {rightFiler}
    </Root>
  );
};

import { css } from "@/components/theme";

export const rootStyle = css`
  display: grid;
  grid-template-columns: 10em 1fr;

  align-items: center;
  padding: ${props => props.theme.spaces.base};
  margin: 0 $baseSpace;

  border-left: ${props => props.theme.spaces.base} solid transparent;

  color: ${props => props.theme.colors.base3};
  background-color: ${props => props.theme.colors.base03};

  transition: background-color ease-out 150ms, color ease-out 150ms;

  &[aria-selected="true"] {
    color: ${props => props.theme.colors.base03};
    background-color: ${props => props.theme.colors.base3};
    border-left: ${props => `${props.theme.spaces.small} solid ${props.theme.colors.red}`};
  }
`;

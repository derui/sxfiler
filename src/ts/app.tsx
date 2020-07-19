import { h } from "preact";
import * as MainContainer from "./components/container/main-container";
import { State } from "@/modules";
import { itemKeys } from "./configurations";
import { selectItem } from "./modules/configuration/selectors";
import { useState, useEffect } from "preact/hooks";
import { selectColorPairs } from "./modules/theme/selectors";

export type Props = {
  state: State;
};

const makeCSSVariables = (colorPairs: { [p: string]: string }): string => {
  const variables = Object.entries(colorPairs).map(([k, v]) => {
    return `--color-${k.replace(".", "-")}: ${v};`;
  });
  return `
:root {
${variables.join("\n")}
}
`;
};

export const Component: preact.FunctionComponent<Props> = ({ state }) => {
  const theme = selectItem(state.configuration, itemKeys.general.theme.currentTheme);

  const [style] = useState(document.createElement("style"));

  useEffect(() => {
    const colorPairs = selectColorPairs(state.theme, theme || "");

    style.innerHTML = makeCSSVariables(colorPairs);
    document.head.appendChild(style);

    return () => {
      document.head.removeChild(style);
    };
  }, [theme]);

  return <MainContainer.Component state={state} />;
};

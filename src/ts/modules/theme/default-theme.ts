// provide default theme.

import { Theme, ColorCode } from "@/generated/theme_pb";

const baseColor = {
  base03: "#002b36",
  base02: "#073642",
  base01: "#586e75",
  base00: "#657b83",
  base0: "#839496",
  base1: "#93a1a1",
  base2: "#eee8d5",
  base3: "#fdf6e3",
  yellow: "#b58900",
  orange: "#cb4b16",
  red: "#dc322f",
  magenta: "#d33682",
  violet: "#6d71c4",
  blue: "#268bd2" /* rgb(38, 139, 210) */,
  cyan: "#2aa198",
  green: "#859900",
};

const colorCode = (key: string, color: string): ColorCode => {
  const code = new ColorCode();
  code.setName(key);
  code.setHexColor(color);
  return code;
};

export const defaultTheme = (): Theme => {
  const theme = new Theme();

  theme.setName("default");
  theme.setDescription("The theme for default");
  theme.setColorCodesList([
    colorCode("ui.switchRail", baseColor.base03),
    colorCode("ui.switchRailChecked", baseColor.blue),
    colorCode("ui.switchBox", baseColor.base3),
    colorCode("completer.candidateText", baseColor.base3),
    colorCode("completer.selectedCandidateBackground", `${baseColor.base2}4d`),
    colorCode("completer.matchingAreaText", baseColor.base03),
    colorCode("completer.matchingAreaBackground", baseColor.yellow),
    colorCode("completer.baseBackground", baseColor.base02),
    colorCode("completer.titleText", baseColor.base03),
    colorCode("completer.titleBackground", baseColor.base3),
    colorCode("completer.inputBorder", baseColor.blue),
    colorCode("completer.inputText", baseColor.base3),
    colorCode("completer.inputBackground", baseColor.base03),

    colorCode("configuration.cellSeparator", baseColor.base3),
    colorCode("configuration.cellBackground", baseColor.base02),
    colorCode("configuration.cellText", baseColor.base3),
    colorCode("configuration.shadowCellFocused", baseColor.orange),
    colorCode("configuration.selectBackground", baseColor.base3),
    colorCode("configuration.dropdownMark", baseColor.base03),
    colorCode("configuration.selectionMenuText", baseColor.base03),
    colorCode("configuration.selectionMenuBackground", baseColor.base3),
    colorCode("configuration.selectionOptionText", baseColor.base3),
    colorCode("configuration.selectionOptionBackground", baseColor.cyan),
    colorCode("configuration.selectionOptionSeparator", baseColor.base01),
    colorCode("configuration.navigatorBackground", baseColor.base02),
    colorCode("configuration.navigatorText", baseColor.base3),
    colorCode("configuration.navigatorCatgegoryMarker", baseColor.base3),
    colorCode("configuration.navigatorSectionText", baseColor.base3),
    colorCode("configuration.navigatorSectionHover", baseColor.orange),
    colorCode("configuration.sectionBackground", baseColor.base03),
    colorCode("configuration.sectionHeaderText", baseColor.base3),
    colorCode("configuration.sectionHeaderBackground", baseColor.base02),
    colorCode("configuration.sectionCellBackground", baseColor.base03),

    colorCode("decisionModal.headerText", baseColor.base03),
    colorCode("decisionModal.headerBackground", baseColor.base3),
    colorCode("decisionModal.panelText", baseColor.base3),
    colorCode("decisionModal.panelBackground", baseColor.base03),
    colorCode("decisionModal.panelSelectedText", baseColor.base03),
    colorCode("decisionModal.panelSelectedBackground", baseColor.base3),
    colorCode("decisionModal.panelSelectedMarker", baseColor.red),

    colorCode("fileItem.baseText", baseColor.base2),
    colorCode("fileItem.baseBackground", baseColor.base03),
    colorCode("fileItem.selectedMarker", baseColor.orange),
    colorCode("fileItem.mark", `${baseColor.blue}3f`),
    colorCode("fileItem.bookmark", baseColor.cyan),
    colorCode("fileItem.directoryText", baseColor.yellow),
    colorCode("fileItem.symlinkText", baseColor.orange),

    colorCode("fileList.baseText", baseColor.base01),
    colorCode("fileList.baseBackground", baseColor.base03),
    colorCode("fileList.emptyText", baseColor.red),
    colorCode("fileList.separator", baseColor.base0),
    colorCode("fileList.headerText", baseColor.base1),
    colorCode("fileList.headerBackground", baseColor.base03),
    colorCode("fileList.headerBorder", baseColor.base01),
    colorCode("fileList.focusedText", baseColor.yellow),

    colorCode("logViewer.text", baseColor.base2),
    colorCode("logViewer.background", baseColor.base03),
    colorCode("logViewer.infoLevel", baseColor.blue),
    colorCode("logViewer.warningLevel", baseColor.orange),
    colorCode("logViewer.errorLevel", baseColor.red),

    colorCode("scrollBar.background", baseColor.base01),

    colorCode("configurationEditor.background", baseColor.base3),
    colorCode("fileList.separator", baseColor.base00),
    colorCode("logViewer.separator", baseColor.base00),
  ]);

  return theme;
};

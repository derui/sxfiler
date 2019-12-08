import { configure } from "@storybook/react";
// automatically import all files ending in *.stories.tsx
const req = require.context("../src/stories", true, /.stories.tsx$/);

function loadStories() {
  req.keys().forEach(req);
}

configure(loadStories, module);

// add root element for modal
if (!document.getElementById("modal-root")) {
  const modalRoot = document.createElement("div");
  modalRoot.setAttribute("id", "modal-root");
  modalRoot.style.position = "absolute";
  modalRoot.style.width = "100%";
  modalRoot.style.height = "100%";
  modalRoot.style.top = "0";
  modalRoot.style.left = "0";

  document.body.append(modalRoot);
}

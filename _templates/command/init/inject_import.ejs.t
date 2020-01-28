---
to: src/ts/commands/index.ts
inject: true
skip_if: \/<%= h.changeCase.camel(name) %>";
after: "import.*"
---
import * as <%= h.changeCase.camel(name) %> from "./<%= h.changeCase.camel(name) %>";
---
to: src/ts/commands/<%= group %>/index.ts
inject: true
skip_if: \/<%= module %>\/<%= name %>
after: "import.*"
---
import * as <%= h.changeCase.camel(`${module}-${name}`) %> from "./<%= module %>/<%= name %>";
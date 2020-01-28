---
to: src/ts/modules/index.ts
inject: true
skip_if: import.+<%= name %>
after: \/\/#IMPORT INDICATOR
---
<%_ const pascalName = h.changeCase.pascal(name) _%>
import * as <%= pascalName %> from "./<%= name %>";
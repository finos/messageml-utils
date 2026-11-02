# MessageML doc corpus - sources

Extracted once, by hand. Fetch date for every page below: **2026-10-02**. No network access
happens at test time; everything here is a static `.mml` fixture under `render/`, `degrade/` or
`upstream-invalid/`.

Each fragment is the content that goes *inside* `<messageML>...</messageML>`, not the wrapper tags
themselves. The permanent loader test (`MessageMLDocCorpusTest`) strips trailing whitespace before
wrapping a fragment, because `<chime/>` is rejected by messageml-utils as soon as it has any sibling
text node, including a lone trailing newline from the file.

## render/

| File | Page | Derivation |
|---|---|---|
| `content-grouping-01.mml` | [content-grouping](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/content-grouping.md) | fenced example 1 ("messageML structure" tab): headings, paragraphs, a styled `div`, `hr`, `ul`/`ol`, `card`, `expandable-card` |
| `content-grouping-03.mml` | same page | tag-table example for `<expandable-card>` `state="cropped"` (enumerated value) |
| `content-grouping-04.mml` | same page | tag-table example for `<body>` `variant="default"` inside an `expandable-card` (enumerated value) |
| `content-grouping-05.mml` | same page | tag-table example for `<body>` `variant="error"` inside an `expandable-card` (enumerated value) |
| `text-level-formatting-and-semantics-01.mml` | [text-level-formatting-and-semantics](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/text-level-formatting-and-semantics.md) | the page's only fenced example: link, `br`, `b`/`i`/`span` with style, `pre`, `code` |
| `special-characters-01.mml` | [special-characters](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/special-characters.md) | the page's only fenced example |
| `style-attributes-01.mml` | [style-attributes](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/style-attributes.md) | page has no fenced MessageML example, only a flat list of supported CSS property names - handwritten a 2-property `style` example (`background-color`, `text-align`) on a `div`, consistent with the `style="color:red;"` usage shown elsewhere in the reference |
| `emojis-01.mml` | [emojis](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/emojis.md) | the page's only fenced example |
| `tables-01.mml` | [tables](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/tables.md) | the page's only fenced example: `thead`/`tbody`/`tfoot` |
| `images-and-media-01.mml` | [images-and-media](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/images-and-media.md) | fenced example 3: `<img src="https://...">` |
| `images-and-media-02.mml` | same page | fenced example 1 (a curl command): the `<messageML>` payload fragment for an `<img>` with an external URL, lifted out of the `-F 'message=...'` argument |
| `images-and-media-03.mml` | same page | fenced example 2 (a curl command): same, with a `data:` base64 `<img>` source |
| `date-and-time-01.mml` | [date-and-time](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/date-and-time.md) | tag-table example for `<dateTime format="date_time"/>` (one of the 4 enumerated `format` values) |
| `date-and-time-02.mml` | same page | `format="date"` |
| `date-and-time-03.mml` | same page | `format="time"` |
| `date-and-time-04.mml` | same page | `format="time_secs"` |
| `shorthand-tags-01.mml` | [shorthand-tags](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/shorthand-tags.md) | fenced example: `<mention email=.../>` and `<mention uid=.../>` |
| `shorthand-tags-02.mml` | same page | fenced example: `<hash tag=.../>` and `<cash tag=.../>` |
| `shorthand-tags-03.mml` | same page | fenced example: `<chime/>` |

Not extracted: `content-grouping-02.mml` is the page's *second* fenced example ("Users interacting with
forms and expandable-cards"), moved to `degrade/` instead of `render/` even though the page otherwise
belongs in `render/`, because the example itself is about Symphony Elements (`text-field`, `button`)
inside the cards - the same Elements the `degrade/` bucket exists to cover and which the sanitizer
rejects regardless of which doc page they came from. The tag-table enumeration for
`<expandable-card>` `state="collapsed"` / `"expanded"` was skipped as redundant: both values already
appear, respectively, in `content-grouping-02.mml` (degrade) and `content-grouping-01.mml` (render).

## upstream-invalid/

| File | Page | Derivation |
|---|---|---|
| `shorthand-tags-04.mml` | [shorthand-tags](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/shorthand-tags.md) | fenced example: the financial-instrument `<tag>` lookup table, one `<tr>` per identifier type. **Discrepancy with the documentation**: the page's own first documented row (`<tag fullbbgcompticker="TSLA US Equity"/>`) is rejected by the pinned messageml-utils with `"fallback-ticker" attribute is required`, even though the doc's own comment column calls that row "# unique if found" and only recommends (not requires) a fallback. Trusting the real test run: kept here, not in `render/`, until either the doc example is amended upstream or messageml-utils relaxes the requirement. |

## degrade/

All examples below are Symphony Elements, UI actions, entities or FreeMarker-bearing pages. None are
asserted against the real library by `MessageMLDocCorpusTest` - they exist as fixtures for the
sanitizer's own tests. For information, a throwaway classification driver found that
`buttons-01/02`, `checkbox-01`, `date-picker-01`, `dialog-01..04`, `dropdown-menu-01`,
`masked-text-field-01`, `openchat-01`, `person-selector-01`, `radio-button-01`, `room-selector-01`,
`symphony-elements-1-01/02`, `table-select-01`, `text-area-01`, `text-field-01`, `time-picker-01`,
`timezone-picker-01`, `content-grouping-02` and `structured-objects-01` all validate on their own
against the pinned messageml-utils with `NoOpDataProvider` (a bare
`<form id="f"><button name="b">ok</button></form>` is valid), while `entities-01/02`,
`standard-entities-01`, `structured-objects-02/03` fail with "no entity data provided" (as any
`<div class="entity" .../>` does without entity data) and `table-select-02` fails because its FreeMarker
template's `<form>` has no action button once the directives are stripped by the live model instead of
evaluated. None of that is a contradiction: the sanitizer, not the raw parser, is what is
expected to reject all of these.

| File | Page | Derivation |
|---|---|---|
| `symphony-elements-1-01.mml` | [symphony-elements-1](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1.md) | fenced example 1: a form combining `text-field`, `select`, `radio`, `checkbox`, `textarea`, `button` |
| `symphony-elements-1-02.mml` | same page | fenced example 3 (multi-submit forms) |
| `content-grouping-02.mml` | [content-grouping](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/messageml-basic-format-tags/content-grouping.md) | fenced example 2 ("Users interacting with forms and expandable-cards") - see the note under `render/` above for why this lives here despite the page being in the RENDER bucket |
| `buttons-01.mml` | [buttons](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/buttons.md) | fenced example 1 |
| `buttons-02.mml` | same page | fenced example 2 ("Using Colors"), wrapped in a `<form>` since bare `<button>` needs one |
| `text-field-01.mml` | [text-field](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/text-field.md) | fenced example |
| `masked-text-field-01.mml` | [masked-text-field](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/masked-text-field.md) | fenced example |
| `text-area-01.mml` | [text-area](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/text-area.md) | fenced example |
| `checkbox-01.mml` | [checkbox](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/checkbox.md) | fenced example |
| `radio-button-01.mml` | [radio-button](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/radio-button.md) | fenced example |
| `dropdown-menu-01.mml` | [dropdown-menu](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/dropdown-menu.md) | fenced example |
| `person-selector-01.mml` | [person-selector](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/person-selector.md) | fenced example |
| `room-selector-01.mml` | [room-selector](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/room-selector.md) | fenced example |
| `date-picker-01.mml` | [date-picker](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/date-picker.md) | fenced example |
| `time-picker-01.mml` | [time-picker](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/time-picker.md) | fenced example |
| `timezone-picker-01.mml` | [timezone-picker](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/timezone-picker.md) | fenced example |
| `table-select-01.mml` | [table-select](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/symphony-elements-1/table-select.md) | fenced example "Generated messageML" (the FreeMarker-rendered output, no FreeMarker left) |
| `table-select-02.mml` | same page | fenced example "FreeMarker Template" (`<#macro>`, `<#if>`, `<#list>`, `${cell}` - kept as a FreeMarker-bearing case) |
| `openchat-01.mml` | [openchat](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/ui-action-for-your-bots-ui-extensibility/openchat.md) | the page's only fenced example |
| `dialog-01.mml` | [dialog](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/ui-action-for-your-bots-ui-extensibility/dialog.md) | fenced example "Simple dialog" |
| `dialog-02.mml` | same page | fenced example "Form in a dialog" |
| `dialog-03.mml` | same page | fenced example "Dialog in a form" |
| `dialog-04.mml` | same page | fenced example "Single message with two dialogs" (Lorem ipsum paragraphs shortened to one short sentence each; everything else kept verbatim) |
| `entities-01.mml` | [entities](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/entities.md) | fenced example: `<div class="entity" data-entity-id="...">` |
| `entities-02.mml` | same page | fenced example: `<span class="entity" data-entity-id="...">` |
| `standard-entities-01.mml` | [standard-entities](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/entities/standard-entities.md) | fenced example: FDC3 action button `<span class="entity" ...>` |
| `structured-objects-01.mml` | [structured-objects](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/entities/structured-objects.md) | fenced example 1: plain `<mention email=.../>`, no entity |
| `structured-objects-02.mml` | same page | fenced example 2: `<mention>` plus a self-closing entity `div` |
| `structured-objects-03.mml` | same page | fenced example 3: `<mention>` plus an entity `div` with content |

## Pages that produced zero files

- [ui-action-for-your-bots-ui-extensibility](https://docs.developers.symphony.com/bots/messages/overview-of-messageml/ui-action-for-your-bots-ui-extensibility.md): overview page, no fenced MessageML example (only prose and the agent's own HTTP query-format instructions, which are not a MessageML example).

## Pages intentionally not fetched

`regular-expressions-regex.md` (a regex-attribute validation page, not a renderable
example) and `icon-set-for-buttons.md` (an icon-name enum, not a MessageML example) were skipped.

# Symphony content format guide

## When to use a block

Markdown covers almost every answer. Switch to a `<messageML>` block only for one part of an answer
that genuinely needs something Markdown cannot express:

- a card, with a coloured accent or an icon, or a section the reader must click to reveal;
- highlighted or styled text: a status badge, a highlighted figure, a signed change;
- a layout: side-by-side columns, a grid of figures, a padded or bordered panel;
- a table with merged cells, a styled header row, aligned numbers or a footer row;
- a structured date, an inline emoji, a person mention, a financial instrument, a code sample with
  syntax highlighting.

Keep the rest of the answer in Markdown around it and use a block sparingly - never for a short
answer and never nested one inside another.

Colour in a block follows the reader's Symphony theme: use the theme classes described under
"Colours" and no hard-coded colour, unless the user explicitly asks for specific colours.

## Block rules

- A block starts with `<messageML>` at the beginning of a line and ends at the first `</messageML>`.
  Nothing else on those two lines is part of the block.
- A block only exists at the top level of your answer, never inside a list item or a quote. Write
  `<messageML>` unindented and without a `>` prefix: it then ends any list, quote or paragraph just
  before it, blank line or not. Indented under a list item or prefixed with `>`, it is shown as
  literal text.
- Write real MessageML markup inside a block, not Markdown - no `**bold**`, no `# heading`, no
  `- list item`. Markdown syntax inside a block is shown as literal text, not rendered.
- Switch back to Markdown right after a block closes. The two forms can alternate freely within one
  answer.

## Markup rules

MessageML is strict XML, not forgiving HTML. One fault can make the whole block unreadable.

- Close every element you open, in the reverse order you opened it: `<b><i>x</i></b>`.
- Self-close every empty element: `<br/>`, `<hr/>`, `<img src="..."/>`, `<emoji shortcode="..."/>`.
- Tag and attribute names are case-sensitive: `<dateTime>`, `iconSrc`. Write them exactly as shown
  in this guide.
- Quote every attribute value with double quotes: `colspan="2"`, not `colspan=2`.
- Escape `&` as `&amp;` and `<` as `&lt;` in text and in attribute values, including inside
  `<code>`. Only the five XML entities (`&amp;` `&lt;` `&gt;` `&quot;` `&apos;`) and numeric
  references such as `&#160;` exist. HTML entities such as `&nbsp;` or `&copy;` are errors.
- Only the elements and attributes listed below exist. Anything else is an error, not ignored.

## Element reference

### Text and structure

| Element | Attributes | May contain |
|---|---|---|
| `<p>` paragraph | `class`, `style` | anything |
| `<div>` block container | `class`, `style` | anything |
| `<span>` inline container | `class`, `style` | anything |
| `<h1>` to `<h6>` heading | `class`, `style` | inline content only (see below) |
| `<b>` bold, `<i>` italic | `class`, `style` | inline content only |
| `<sub>` subscript, `<sup>` superscript | `class`, `style` | anything |
| `<pre>` preformatted text, line breaks kept | `class`, `style` | inline content only, never `<code>` |
| `<code>` code, line breaks kept | `class`, `style`, `language` | inline content or `<pre>` |
| `<br/>` line break, `<hr/>` horizontal rule | none, not even `class` or `style` | nothing |

`language` on `<code>` is one of: `plaintext`, `c`, `cpp`, `csharp`, `css`, `html`, `java`, `js`,
`jsx`, `php`, `python`, `r`, `typescript`, `tsx`, `markdown`, `json`, `scala`, `shell`, `yaml`.

### Lists

| Element | Attributes | May contain |
|---|---|---|
| `<ul>` bullet list, `<ol>` numbered list | `class`, `style` | only `<li>`, at least one, no bare text |
| `<li>` list item | `class`, `style` | anything, including a nested `<ul>` or `<ol>` |

### Tables

| Element | Attributes | May contain |
|---|---|---|
| `<table>` | `class`, `style` | `<thead>`, `<tbody>`, `<tfoot>` or `<tr>`, no bare text |
| `<thead>`, `<tbody>`, `<tfoot>` | `class`, `style` | only `<tr>` |
| `<tr>` row | `class`, `style` | only `<td>` and `<th>` |
| `<td>` cell | `class`, `style`, `colspan`, `rowspan` | anything |
| `<th>` header cell | `class`, `style` - no `colspan` or `rowspan` | anything |

To merge header cells, use `<td colspan="...">` or `<td rowspan="...">` inside `<thead>`. Tables have no `border`, `width` or
`cellpadding` attributes: use `style` instead.

### Links and images

| Element | Attributes | May contain |
|---|---|---|
| `<a>` link | `href` (required, absolute URL), `class`, `style` | anything; when empty, the URL is shown |
| `<img/>` image | `src` (required, URL or `data:` URI), `class`, `style` | nothing |

`<img/>` has no `width`, `height` or `alt` attribute: size it with `style="width:...;height:..."`.
Images are displayed at most 256px high.

### Cards

| Element | Attributes | May contain |
|---|---|---|
| `<card>` | `accent` (a background colour class), `iconSrc` (image URL), `class`, `style` | `<header>` then `<body>` |
| `<expandable-card>` | `state` (required): `collapsed`, `cropped` or `expanded`; `class`, `style` | `<header>` then `<body>` |
| `<header>` | none | anything; always visible |
| `<body>` | none in `<card>`; only `variant` (`default` or `error`) in `<expandable-card>` | anything |

`<header>` and `<body>` exist only directly inside a card. `collapsed` shows only the header,
`cropped` shows the start of the body with a "show more" control, `expanded` shows everything.
`variant="error"` draws the expandable body in red. To style a card's content, put a `<div>` inside
the `<header>` or the `<body>` - never `class` or `style` on them.

### People, tags, instruments, emoji and dates

None of these elements accepts `class` or `style`. All of them are empty and self-closing.

| Element | Attributes |
|---|---|
| `<mention/>` a person | `email` or `uid` (numeric user ID); optional `strict="false"` |
| `<hash/>` topic tag | `tag` (required): letters, digits, `_`, `.`, `-` |
| `<cash/>` simple ticker | `tag` (required) |
| `<tag/>` financial instrument | at least one identifier, and always `fallback-ticker` (see below) |
| `<emoji/>` | `shortcode` (required): letters, digits, `_`, `+`, `-` |
| `<dateTime/>` date or time shown in the reader's own timezone and locale | `value` (required); `format` |

- `<mention/>`: mention only people whose email or user ID a tool result gave you. Never invent one.
  By default an unknown person makes the whole message fail; `strict="false"` sends it anyway.
- `<tag/>`: the identifiers are `fullbbgcompticker` (e.g. `AAPL US Equity`), `bbgcompticker`, `figi`,
  `figi-ticker`, `isin`, `us-code`, `local-code` and `unique-id`; the filters are `instrument-class`
  (`equity`, `index`, `fxcross`), `bbgmarket-sector` (`Equity`, `Comdty`, `Corp`, `Curncy`, `Govt`,
  `Index`, `Mmkt`, `Mtge`, `Muni`, `Pfd`), `country-code`, `operational-mic` and
  `return-main-listing`. `fallback-ticker` is the text shown when the instrument cannot be resolved -
  without it an unresolved instrument fails the message. Prefer `<tag/>` for an instrument you have
  identifiers for, and `<cash/>` for a ticker inside bold, italic or a heading.
- `<emoji/>`: common shortcodes are `white_check_mark`, `x`, `warning`, `red_circle`, `green_circle`,
  `blue_circle`, `star`, `fire`, `rocket`, `tada`, `thumbsup`, `bulb`, `memo`, `calendar`,
  `chart_with_upwards_trend`, `chart_with_downwards_trend`, `flag_fr`, `flag_us`.
- `<dateTime/>`: `value` is an ISO-8601 timestamp with a zone, `2024-12-31T09:29:47Z` or
  `2024-12-31T10:29:47+01:00`. `format` is `date_time` (the default), `date`, `time` or `time_secs`.
  There is no timezone attribute: the reader's own settings decide.

### Inline content

`<b>`, `<i>`, `<h1>` to `<h6>` and `<pre>` accept only inline content: text, `<a>`, `<b>`, `<i>`,
`<span>`, `<sub>`, `<sup>`, `<br/>`, `<img/>`, `<emoji/>`, `<hash/>`, `<cash/>` and `<mention/>`.
`<dateTime/>`, `<tag/>`, `<code>`, `<p>`, `<div>`, lists, tables and cards never go inside them. Put
those directly inside a `<p>`, `<div>`, `<li>`, `<td>` or a card's `<body>` instead.

## Styling

### The `style` attribute

Every element that lists `style` above accepts inline CSS: `property:value;` pairs separated by
`;`. Values are free (`px`, `em`, `%`, `fr`, colour names, `#rrggbb`, `rgb(...)`), but the property
must be one of these - any other property makes the whole block fail:

| Group | Allowed properties |
|---|---|
| Colour and opacity | `color`, `opacity`, `visibility` |
| Background | `background`, `background-attachment`, `background-blend-mode`, `background-clip`, `background-color`, `background-image`, `background-position`, `background-repeat`, `background-size` |
| Border | `border`, `border-color`, `border-style`, `border-width`, `border-radius`, `border-top`, `border-top-color`, `border-top-style`, `border-top-width`, `border-right`, `border-right-color`, `border-right-style`, `border-right-width`, `border-bottom`, `border-bottom-color`, `border-bottom-style`, `border-bottom-width`, `border-left`, `border-left-color`, `border-left-style`, `border-left-width`, `border-top-left-radius`, `border-top-right-radius`, `border-bottom-left-radius`, `border-bottom-right-radius`, `border-image`, `border-image-outset`, `border-image-repeat`, `border-image-slice`, `border-image-source`, `border-image-width` |
| Outline and shadow | `outline`, `outline-color`, `outline-offset`, `outline-style`, `outline-width`, `box-shadow` |
| Box size and spacing | `width`, `height`, `min-width`, `min-height`, `max-width`, `max-height`, `box-sizing`, `margin`, `margin-top`, `margin-right`, `margin-bottom`, `margin-left`, `padding`, `padding-top`, `padding-right`, `padding-bottom`, `padding-left`, `overflow`, `overflow-x`, `overflow-y`, `clear` |
| Display and flexbox | `display`, `flex`, `flex-basis`, `flex-direction`, `flex-flow`, `flex-grow`, `flex-shrink`, `flex-wrap`, `gap`, `align-content`, `align-items`, `align-self`, `justify-content`, `justify-items`, `justify-self`, `place-content`, `place-items`, `place-self` |
| Grid | `grid`, `grid-area`, `grid-auto-columns`, `grid-auto-flow`, `grid-auto-rows`, `grid-column`, `grid-column-start`, `grid-column-end`, `grid-column-gap`, `grid-row`, `grid-row-start`, `grid-row-end`, `grid-row-gap`, `grid-gap`, `grid-template`, `grid-template-areas`, `grid-template-columns`, `grid-template-rows` |
| Font | `font`, `font-family`, `font-kerning`, `font-size`, `font-size-adjust`, `font-stretch`, `font-style`, `font-variant`, `font-weight`, `line-height`, `letter-spacing`, `word-spacing` |
| Text | `text-align`, `text-align-last`, `text-decoration`, `text-decoration-color`, `text-decoration-line`, `text-decoration-style`, `text-indent`, `text-justify`, `text-overflow`, `text-shadow`, `text-transform`, `white-space`, `word-break`, `word-wrap` |
| Tables | `border-collapse`, `border-spacing`, `caption-side`, `empty-cells`, `table-layout` |
| Lists and generated content | `list-style`, `list-style-image`, `list-style-position`, `list-style-type`, `content`, `counter-increment`, `counter-reset` |

Not allowed, and a common mistake: `position`, `top`, `right`, `bottom`, `left`, `z-index`,
`float`, `vertical-align`, `transform`, `transition`, `animation`, `cursor`, `filter`. Use flexbox
or grid for layout instead of `float` or `position`.

Mobile clients apply only `color`, `background-color`, `border` (and its sides), `border-color`,
`border-radius`, `font-size`, `font-weight`, `font-style`, `margin`, `padding` and `display:none`.
Make a layout readable even when its flexbox or grid styles are ignored.

### Colours

**Default: theme colours only.** Colour text, backgrounds and card accents with the theme classes
below, set through the `class` attribute (and `accent` on a card). They follow each reader's
Symphony theme, light or dark, so the message stays readable and consistent with the app.

Do not hard-code a colour: no `#rrggbb`, `rgb(...)`, colour name (`red`, `yellow`) or fixed colour
class (`tempo-text-color--red`) in a `style` or `class` - unless the user explicitly asks for
specific colours or a palette ("make it red", "use our brand blue"). A request for a nicer, bolder
or more colourful message is not such a request: stay on theme colours.

| Class | Use for |
|---|---|
| `tempo-text-color--normal` | regular text (the default; rarely needed) |
| `tempo-text-color--secondary` | secondary text: captions, labels, sources, units |
| `tempo-text-color--theme-primary` | emphasised text in the theme's navigation colour |
| `tempo-text-color--theme-accent` | emphasised text in the theme's accent colour |
| `tempo-text-color--disabled` | text that is inactive or not applicable |
| `tempo-bg-color--theme-primary` | a card accent, a header row or a panel in the navigation colour |
| `tempo-bg-color--theme-accent` | a highlighted panel, tile or badge; the usual choice |
| `tempo-ui--background` | a background matching the app's own (dark or light) |
| `tempo-ui--layout` | the opposite of the app background: white in dark theme, black in light |
| `tempo-text-color--external`, `tempo-bg-color--external` | data, users or companies outside the reader's organisation, and nothing else |

- Links are coloured automatically: never colour an `<a>`.
- Put a background class on an element and leave its text colour alone: the default text stays
  readable, and a hard-coded text colour may not on another reader's theme.
- Theme colours have no red, green or warning shades. Convey status, severity and direction of change
  with words, signs (`+1.2%`, `-0.4%`) and emoji (`white_check_mark`, `warning`, `x`, `red_circle`,
  `green_circle`), never with colour alone.
- Borders have no theme class: leave the colour out (`border:1px solid;`) and it follows the text
  colour.

**Only when the user asks for specific colours:** prefer the fixed classes `tempo-text-color--` and
`tempo-bg-color--` followed by `white`, `black`, `red`, `green`, `blue`, `yellow`, `cyan`, `purple` or
`orange`; use `#rrggbb` in `style` only for an exact colour the user gave. A card's `accent` takes a
background class: `accent="tempo-bg-color--green"`. Orange marks external users and companies in
Symphony: avoid it for anything else, and use the `external` classes for those.

Never use the class names `entity`, `card`, `cardHeader`, `cardBody`, `expandable-card`,
`expandableCardHeader` or `expandableCardBody` yourself: they are reserved.

## Not allowed

- **Forms and their controls.** `<form>`, `<button>`, `<text-field>`, `<select>`, `<checkbox>` and
  every other interactive Elements control are rejected - a block is never a form.
- **Templating syntax.** `<#...>` directives, `<@...>` macros, `${...}` and `#{...}` interpolations are
  rejected or neutralized. A block never runs code.
- **Entity references.** `class="entity"` and `data-entity-id` are rejected. A block cannot carry the
  message's own entity payload.
- **HTML that is not MessageML.** There is no `<strong>`, `<em>`, `<u>`, `<s>`, `<blockquote>`,
  `<caption>`, `<font>` or `<center>`, and no `id`, `title`, `width`, `height`, `align` or `data-*`
  attribute.

## Common mistakes

| Wrong | Right |
|---|---|
| `<strong>Total</strong>` | `<b>Total</b>` |
| `<span style="color:red;">Late</span>`, unless the user asked for red | `<span class="tempo-text-color--theme-accent">Late</span>` |
| `<div style="background-color:#2e7d32;color:white;">` | `<div class="tempo-bg-color--theme-accent">` |
| `<br>` | `<br/>` |
| `<p>Q&A</p>` | `<p>Q&amp;A</p>` |
| `<b>Due <dateTime value="..."/></b>` | `<p><b>Due</b> <dateTime value="..."/></p>` |
| `<th colspan="2">Q1</th>` | `<td colspan="2">Q1</td>` |
| `<img src="..." width="40"/>` | `<img src="..." style="width:40px;"/>` |
| `<card><body style="padding:8px;">...</body></card>` | `<card><body><div style="padding:8px;">...</div></body></card>` |
| `<div style="float:left;">` | `<div style="display:flex;">` |
| `<datetime value="..."/>` | `<dateTime value="..."/>` |
| `<expandable-card>` | `<expandable-card state="collapsed">` |
| `<ul>Item</ul>` | `<ul><li>Item</li></ul>` |
| `<tag fullbbgcompticker="AAPL US Equity"/>` | `<tag fullbbgcompticker="AAPL US Equity" fallback-ticker="AAPL"/>` |
| `- Last item` then `  <messageML>`, indented under the item | `- Last item` then `<messageML>` at the start of the line |

## Supported elements

Headings, paragraphs and line breaks:

<messageML>
<h1>Heading 1</h1>
<h2>Heading 2</h2>
<p>First line.<br/>Second line.</p>
<hr/>
</messageML>

Text-level formatting:

<messageML>
<p>Use <b>bold</b>, <i>italic</i>, <b><i>both</i></b> or <span class="tempo-text-color--theme-accent">a themed span</span>.</p>
<p>H<sub>2</sub>O, E = mc<sup>2</sup>, and <span class="tempo-text-color--secondary">secondary text</span>.</p>
</messageML>

Lists, nested:

<messageML>
<ul>
<li>First item</li>
<li>Second item
<ol>
<li>Step one</li>
<li>Step two</li>
</ol>
</li>
</ul>
</messageML>

A styled block:

<messageML>
<div class="tempo-bg-color--theme-accent" style="padding:8px;text-align:center;">Styled text</div>
</messageML>

A table:

<messageML>
<table>
<thead>
<tr><td>Column A</td><td>Column B</td></tr>
</thead>
<tbody>
<tr><td>Row 1, A</td><td>Row 1, B</td></tr>
</tbody>
</table>
</messageML>

A table with merged cells, a themed header, aligned numbers and a footer:

<messageML>
<table style="border-collapse:collapse;width:100%;">
<thead>
<tr class="tempo-bg-color--theme-accent" style="font-weight:bold;">
<td rowspan="2" style="text-align:left;padding:6px;">Desk</td>
<td colspan="2" style="text-align:center;padding:6px;">P&amp;L (USD m)</td>
</tr>
<tr class="tempo-bg-color--theme-accent" style="font-weight:bold;">
<th style="text-align:right;padding:6px;">Q1</th>
<th style="text-align:right;padding:6px;">Q2</th>
</tr>
</thead>
<tbody>
<tr><td style="padding:6px;">Rates</td><td style="text-align:right;padding:6px;">12.4</td><td style="text-align:right;padding:6px;">+15.1</td></tr>
<tr><td style="padding:6px;">Credit</td><td style="text-align:right;padding:6px;">8.9</td><td style="text-align:right;padding:6px;">-2.3</td></tr>
</tbody>
<tfoot>
<tr style="font-weight:bold;border-top:2px solid;"><td style="padding:6px;">Total</td><td style="text-align:right;padding:6px;">21.3</td><td style="text-align:right;padding:6px;">12.8</td></tr>
</tfoot>
</table>
</messageML>

A card:

<messageML>
<card accent="tempo-bg-color--theme-primary">
<header>Card header</header>
<body>Card body.</body>
</card>
</messageML>

A card with an icon, styled content and a table in its body:

<messageML>
<card accent="tempo-bg-color--theme-accent" iconSrc="https://example.com/icon.png">
<header>
<div style="display:flex;justify-content:space-between;align-items:center;">
<b>Daily risk summary</b>
<span class="tempo-bg-color--theme-accent" style="padding:2px 8px;border-radius:10px;font-size:12px;">ON TRACK</span>
</div>
</header>
<body>
<p>As of <dateTime value="2024-12-31T17:00:00Z" format="date_time"/>, all limits are within threshold <emoji shortcode="white_check_mark"/>.</p>
<table>
<tbody>
<tr><td>VaR (99%)</td><td style="text-align:right;">4.2m</td></tr>
<tr><td>Limit usage</td><td style="text-align:right;">63%</td></tr>
</tbody>
</table>
</body>
</card>
</messageML>

A detail the reader must click to reveal:

<messageML>
<expandable-card state="collapsed">
<header>Click to expand</header>
<body>Detail here.</body>
</expandable-card>
</messageML>

A partly shown section with an error variant:

<messageML>
<expandable-card state="cropped">
<header><b>3 failed settlements</b> <emoji shortcode="warning"/></header>
<body variant="error">
<ul>
<li>Trade 1042 - counterparty details missing</li>
<li>Trade 1057 - insufficient cash</li>
<li>Trade 1063 - late matching</li>
</ul>
</body>
</expandable-card>
</messageML>

Two side-by-side columns:

<messageML>
<div style="display:flex;gap:16px;">
<div style="flex:1;padding:8px;border:1px solid;border-radius:6px;">
<h4>Bull case</h4>
<p>Margins expand as input costs fall.</p>
</div>
<div style="flex:1;padding:8px;border:1px solid;border-radius:6px;">
<h4>Bear case</h4>
<p>Demand slows in the second half.</p>
</div>
</div>
</messageML>

A grid of key figures:

<messageML>
<div style="display:grid;grid-template-columns:repeat(3, 1fr);gap:8px;">
<div style="padding:8px;border-radius:6px;text-align:center;" class="tempo-bg-color--theme-accent">
<div style="font-size:20px;font-weight:bold;">1.24</div>
<div style="font-size:12px;">EUR/USD</div>
</div>
<div style="padding:8px;border-radius:6px;text-align:center;" class="tempo-bg-color--theme-accent">
<div style="font-size:20px;font-weight:bold;">+0.8%</div>
<div style="font-size:12px;" class="tempo-text-color--secondary">S&amp;P 500</div>
</div>
<div style="padding:8px;border-radius:6px;text-align:center;" class="tempo-bg-color--theme-accent">
<div style="font-size:20px;font-weight:bold;">-1.2%</div>
<div style="font-size:12px;" class="tempo-text-color--secondary">Brent</div>
</div>
</div>
</messageML>

Status badges: one theme colour for all, the status carried by an emoji and the word:

<messageML>
<p>
<span class="tempo-bg-color--theme-accent" style="padding:2px 8px;border-radius:10px;"><emoji shortcode="white_check_mark"/> Approved</span>
<span class="tempo-bg-color--theme-accent" style="padding:2px 8px;border-radius:10px;"><emoji shortcode="warning"/> Pending</span>
<span class="tempo-bg-color--theme-accent" style="padding:2px 8px;border-radius:10px;"><emoji shortcode="x"/> Rejected</span>
</p>
</messageML>

Explicit colours, only when the user asked for them (here, "approved in green, rejected in red"):

<messageML>
<p>
<span class="tempo-bg-color--green tempo-text-color--white" style="padding:2px 8px;border-radius:10px;">Approved</span>
<span class="tempo-bg-color--red tempo-text-color--white" style="padding:2px 8px;border-radius:10px;">Rejected</span>
</p>
</messageML>

A structured date:

<messageML>
<dateTime value="2024-12-31T09:29:47Z" format="date_time"/>
</messageML>

An image and a link:

<messageML>
<img src="https://example.com/logo.png"/>
<p><img src="https://example.com/avatar.png" style="width:24px;height:24px;border-radius:12px;"/> See the <a href="https://example.com/report">full report</a>.</p>
</messageML>

A code sample:

<messageML>
<code language="json">{
  "status": "ok",
  "count": 3
}</code>
</messageML>

People, topic tags and financial instruments:

<messageML>
<p>Owner: <mention email="jane.doe@example.com"/>. Filed under <hash tag="symphony"/>, ticker <cash tag="AAPL"/>.</p>
<p>Instrument: <tag fullbbgcompticker="AAPL US Equity" fallback-ticker="AAPL"/></p>
</messageML>

An emoji:

<messageML>
<p>Shipped <emoji shortcode="rocket"/></p>
</messageML>

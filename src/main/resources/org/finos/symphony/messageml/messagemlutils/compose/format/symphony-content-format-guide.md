# Symphony content format guide

## When to use a block

Markdown covers almost every answer. Switch to a `<messageML>` block only for one part of an answer
that genuinely needs something Markdown cannot express: a card, a coloured span, a structured date,
an inline emoji. Keep the rest of the answer in Markdown around it and use a block sparingly - never
for a short answer and never nested one inside another.

## Block rules

- A block starts with `<messageML>` at the beginning of a line and ends at the first `</messageML>`.
  Nothing else on those two lines is part of the block.
- A block only exists at the top level of your answer, never inside a list item or a quote.
- Write real MessageML markup inside a block, not Markdown - no `**bold**`, no `# heading`, no
  `- list item`. Markdown syntax inside a block is shown as literal text, not rendered.
- Switch back to Markdown right after a block closes. The two forms can alternate freely within one
  answer.

## Not allowed

- **Forms and their controls.** `<form>`, `<button>`, `<text-field>`, `<select>`, `<checkbox>` and
  every other interactive Elements control are rejected - a block is never a form.
- **Templating syntax.** `<#...>` directives, `<@...>` macros, `${...}` and `#{...}` interpolations are
  rejected or neutralized. A block never runs code.
- **Entity references.** `class="entity"` and `data-entity-id` are rejected. A block cannot carry the
  message's own entity payload.

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
<p>Use <b>bold</b>, <i>italic</i> or <span style="color:red;">a coloured span</span>.</p>
</messageML>

Lists:

<messageML>
<ul>
<li>First item</li>
<li>Second item</li>
</ul>
</messageML>

A styled block:

<messageML>
<div style="background-color:yellow;text-align:center;">Styled text</div>
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

A card:

<messageML>
<card accent="tempo-bg-color--blue">
<header>Card header</header>
<body>Card body.</body>
</card>
</messageML>

A detail the reader must click to reveal:

<messageML>
<expandable-card state="collapsed">
<header>Click to expand</header>
<body>Detail here.</body>
</expandable-card>
</messageML>

A structured date:

<messageML>
<dateTime value="2024-12-31T09:29:47Z" format="date_time"/>
</messageML>

An image:

<messageML>
<img src="https://example.com/logo.png"/>
</messageML>

Topic tags and financial instruments:

<messageML>
<p>Filed under <hash tag="symphony"/>, ticker <cash tag="AAPL"/>.</p>
</messageML>

An emoji:

<messageML>
<p>Shipped <emoji shortcode="rocket"/></p>
</messageML>

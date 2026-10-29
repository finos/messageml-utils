# extended-markdown-rendering Specification

## Purpose

Lets any MessageML producer write extended Markdown and get MessageML that the library accepts. The Markdown can carry Symphony entities, card and expandable-card groupings, inline native MessageML blocks and Symphony deep links. The capability also turns stored messages back into readable text.

## Requirements

### Requirement: Render extended Markdown to MessageML
The library SHALL convert a Markdown string into a MessageML body, or a full `<messageML>` document, under per-call render options. The converter SHALL be stateless and safe to share across threads. A `null` input SHALL produce an empty result, not an error. Every document produced from well-formed options SHALL be accepted by the library's own MessageML parser.

#### Scenario: Standard Markdown
- **WHEN** Markdown with headings, emphasis, lists, links, images, tables, fenced and indented code and thematic breaks is rendered as a document
- **THEN** the output uses the matching MessageML elements and parses without error in `MessageMLContext`

#### Scenario: Special characters and templating sigils are neutralized
- **WHEN** the Markdown contains `<`, `&`, `${...}` or `#{...}` in text or code
- **THEN** they are emitted as escaped text and never reach the template engine as an interpolation or directive

#### Scenario: Null input
- **WHEN** `null` is rendered
- **THEN** the result is empty and no exception is thrown

### Requirement: Render options are explicit and default to legacy behavior
Rendering SHALL take an immutable options value with four switches: entity syntax, content grouping, intermediate (streaming) rendering and native MessageML blocks. A `LEGACY` preset SHALL disable all four. A `full()` preset SHALL enable entity syntax and content grouping only. Derived copies SHALL toggle native blocks and intermediate or final state without changing the other switches.

#### Scenario: Legacy preset
- **WHEN** Markdown containing `#[tag]` and a `:::card` directive is rendered with `LEGACY`
- **THEN** both stay literal text, and a Markdown blockquote is still emitted as `<card><body>`

### Requirement: Symphony entity syntax
With entity syntax enabled, the converter SHALL turn `#[tag]` into `<hash tag="tag"/>`, `$[TICKER]` into `<cash tag="TICKER"/>`, and bracketed mentions into `<mention/>`: `@[Name](mailto:email)` or `@[Name](email)` gives `email="..."`, and `@[Name](sym:user/<id>)` gives `uid="..."`. Bare mention, hashtag and uppercase cashtag tokens SHALL be recognized whatever the toggle. Entity forms inside inline code, fenced code, link destinations, or after a sigil inside a word SHALL stay verbatim. Empty, whitespace-containing or unterminated forms SHALL stay literal text.

#### Scenario: Explicit hashtag
- **WHEN** `Filed under #[quarterly-review] today.` is rendered with entity syntax enabled
- **THEN** the document is `<messageML>Filed under <hash tag="quarterly-review"/> today.</messageML>`

#### Scenario: Mention without the toggle
- **WHEN** `Hello @[John Smith](john.smith@company.com), please review this.` is rendered with `LEGACY`
- **THEN** the document is `<messageML>Hello <mention email="john.smith@company.com"/>, please review this.</messageML>`

#### Scenario: Entity form in code
- **WHEN** `#[tag]` appears inside inline or fenced code
- **THEN** it is rendered as code text, not as a `<hash/>` element

### Requirement: Rendered form counts
A render call SHALL be able to return the body together with counts of the entity forms it actually produced, and of native block outcomes on final renders. A form that degraded to text SHALL NOT be counted. Counts SHALL be empty only when nothing was produced. Intermediate renders SHALL report no native block counts.

#### Scenario: Ordinary prose
- **WHEN** Markdown with no entity forms is rendered
- **THEN** the reported counts are empty

### Requirement: Container directives
With content grouping enabled, a top-level `:::card` block SHALL render as `<card>`. A leading heading, or a title on the opening line, SHALL become the `<header>`. Only the supported attributes (accent, absolute icon URL) SHALL be kept, with quotes removed, and the body keeps rich Markdown. A `:::expand` (or `:::expandable`) block SHALL render as `<expandable-card>` with `state="collapsed"` by default. `expanded` and `cropped` modifiers set the state, `error` sets the body variant, and unknown modifiers fall back to collapsed. With grouping enabled, a blockquote SHALL render as emphasized text, not a card. A directive nested in another directive SHALL degrade to literal text. An unterminated directive SHALL close at the end of input. An indented directive SHALL be code. On intermediate renders, an expandable card SHALL be forced to `state="expanded"`.

#### Scenario: Nested directive
- **WHEN** a `:::card` block contains another `:::card` line
- **THEN** the inner directive is emitted as literal text inside the outer card body

#### Scenario: Grouping disabled
- **WHEN** a `:::card` block is rendered with content grouping disabled
- **THEN** the `:::` lines are literal text and the body around them is still rendered

### Requirement: Native MessageML blocks inside Markdown
A top-level region that starts with a `<messageML>` line and ends at the first `</messageML>` SHALL be treated as a native block. With native blocks enabled, each block SHALL go through the fragment repair pipeline and its result SHALL be inlined in place. Markdown before and after the block SHALL render normally, and the whole answer SHALL form one valid document. With native blocks disabled, a block SHALL render as escaped literal text, not be dropped. One unrepairable block SHALL degrade only itself.

#### Scenario: Mixed answer
- **WHEN** Markdown contains two valid native blocks separated by Markdown paragraphs and native blocks are enabled
- **THEN** a single valid document is produced with both blocks inlined and the paragraphs rendered around them

### Requirement: Symphony deep links
The library SHALL build canonical Symphony message and room links from stream, message and stream-type IDs. It SHALL accept standard, URL-safe or already percent-encoded IDs and SHALL never double-encode. It SHALL return `null` for an invalid ID or unknown stream type. It SHALL normalize an existing link: upgrade the legacy scheme, canonicalize host variants, re-encode IDs, drop a stray stream type on message links, keep other parameters byte for byte, and be idempotent. Links that are not Symphony deep links SHALL be untouched. The converter SHALL apply this normalization to Markdown link destinations.

#### Scenario: Idempotent normalization
- **WHEN** an already canonical message link is normalized
- **THEN** the same string is returned

#### Scenario: Unknown room type
- **WHEN** a room link is requested with an unknown or `null` stream type
- **THEN** `null` is returned

### Requirement: Plain-text helpers for stored messages
The library SHALL reduce Symphony extended Markdown to plain Markdown. This removes directive markers while keeping headings and titles, reduces entity forms to their display text, keeps a mailto address after its label unless the label already shows it, and replaces each native block with a rich-content marker. It SHALL handle Windows and lone carriage-return line endings and leave fenced examples untouched. The library SHALL also separate card and expandable-card header text from body text in stored PresentationML, so that text extraction does not fuse them, without changing which words survive. Both helpers SHALL accept `null` and empty input.

#### Scenario: Expand directive to plain Markdown
- **WHEN** a `:::expand Title` block is reduced to plain Markdown
- **THEN** the result is a bold `Title` label followed by its body, with no `:::` markers left

#### Scenario: Card header separation
- **WHEN** stored PresentationML with a card header and body is passed through the separator and then text-extracted
- **THEN** the header and body text appear as separate runs

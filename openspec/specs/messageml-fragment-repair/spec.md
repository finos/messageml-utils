# messageml-fragment-repair Specification

## Purpose

Lets a caller check untrusted or machine-generated MessageML fragments, repair them deterministically, or degrade them safely before sending. The caller gets a valid document or a predictable fallback, never a rejected message.

## Requirements

### Requirement: Structural validation against the library
The library SHALL validate a MessageML document, together with its optional entity JSON payload, using the library's own parser with a data provider that does no lookups. It SHALL return `null` when valid, and the parser's error message otherwise. Empty content SHALL be reported as an error without parsing. An entity `div` without its payload SHALL fail, and SHALL pass when the payload is supplied. The library SHALL map an error to one of a bounded set of reason labels: `missing_entity_data`, `invalid_attribute_value`, `disallowed_element`, `malformed_markup`, `other`.

#### Scenario: Entity div needs its payload
- **WHEN** a document with `<div class="entity" data-entity-id="x"/>` is validated without entity JSON
- **THEN** an error is returned
- **AND** validating it with the matching entity JSON returns `null`

### Requirement: Error-directed repair
Given a document and its validation error, the library SHALL return a deterministic rewrite that addresses that error, or `null` when there is no error, the markup is `null`, or no rule applies. Repairs SHALL include closing unterminated elements (also in a rootless body), unwrapping disallowed elements while keeping their text, dropping attributes not allowed on an element, and unwrapping block elements inside phrasing-only elements. They SHALL also escape stray `&` while leaving real entity references alone, unwrap non-absolute anchors while keeping absolute ones, give an expandable card a missing or invalid state, and unwrap orphaned or misplaced grouping tags while keeping header and body content.

#### Scenario: Disallowed element
- **WHEN** a document containing an element MessageML does not allow is repaired with its validation error
- **THEN** the element is removed and its text content is kept

#### Scenario: Nothing applies
- **WHEN** repair is asked for an error no rule handles
- **THEN** `null` is returned

### Requirement: Escaped fallback
The library SHALL build a fallback document from plain text. The text SHALL be fully escaped, with `$` and `#` of templating sigils written as numeric references, and any entity `div` of the original markup SHALL be preserved. The result SHALL always be a valid document. `null` inputs SHALL give an empty document.

#### Scenario: Escaping sigils
- **WHEN** the fallback is built from `Body <of> the answer, ${7*7} and #{x}`
- **THEN** it is `<messageML>Body &lt;of&gt; the answer, &#36;{7*7} and &#35;{x}</messageML>` and validates

### Requirement: Fragment sanitization
The library SHALL sanitize a fragment before rendering. It SHALL reject any templating directive or macro call (`<#...>`, `</#...>`, `<@...>`, `</@...>`) anywhere, including attribute values, comments, CDATA and unfinished tails. It SHALL reject every Symphony Elements form tag in any letter case, any `class="entity"` (any quoting or case), and any `data-entity-id` attribute. Ordinary class names that contain the word "entity" SHALL be allowed. It SHALL escape `${` and `#{` interpolations in text and attribute values. The output SHALL never contain a sequence that starts the template engine. The list of rejected form tags SHALL come from the library's own element set, not a hand-maintained copy.

#### Scenario: Templating directive
- **WHEN** a fragment contains `<#assign x=1>`
- **THEN** sanitization rejects the fragment

#### Scenario: Plain fragment
- **WHEN** a fragment has no forbidden content
- **THEN** it is returned unchanged apart from escaped interpolations

### Requirement: Streaming-safe balancing
The library SHALL balance a fragment that may be cut short. It SHALL drop a tag that is cut mid-way, prune freshly opened empty list items and lists, close open elements innermost first, self-close void elements, escape an unterminated entity reference, and cut back an unterminated comment or CDATA. It SHALL NOT auto-close mismatched or out-of-order tags, which stay for the structural repairer. An already balanced fragment SHALL be returned as the same instance. `null` SHALL return `null`.

#### Scenario: Truncated inline element
- **WHEN** `<p>Hello <b>wor` is balanced
- **THEN** the result closes `b` then `p`

#### Scenario: Mismatch is not guessed
- **WHEN** a fragment has a closing tag that does not match the open element
- **THEN** balancing does not invent a closing order

### Requirement: Lenient structural repair
The library SHALL rebuild a well-formed fragment out of tag soup in a deterministic way. It SHALL apply implicit closes for list items, rows, cells, card sections and paragraphs, insert missing table and list wrappers, close phrasing elements before block elements and reopen bold and italic around the remaining text, and self-close void elements. It SHALL remove empty lists, make text XML-safe (raw `<`, stray `&`, named HTML references turned numeric, CDATA turned to text, comments dropped), and normalize element names and their HTML synonyms. It SHALL make attributes well-formed (quote values, drop duplicates and value-less non-boolean attributes) and strip colons from emoji shortcodes. An upper-case form tag SHALL NOT be lowercased into a real form element.

#### Scenario: Close through the stack
- **WHEN** `<p><b>world</p>` is repaired
- **THEN** the result is `<p><b>world</b></p>`

### Requirement: Native block pipeline with degrade
The library SHALL render one native block through sanitize, balance, validate, structural repair and error-directed repair. It SHALL report an outcome of `RENDERED`, `REPAIRED` or `DEGRADED`. On a final render, a block that is rejected or cannot be repaired SHALL become a collapsed expandable card with a warning header that shows the original source as escaped text. On an intermediate render it SHALL become exactly the placeholder `<i>✨ Generating rich content…</i>`. On intermediate renders, collapsed expandable cards SHALL be shown expanded. On final renders the authored state SHALL be kept.

#### Scenario: Rejected block while streaming
- **WHEN** the block `<#assign x=1>` is rendered as an intermediate update
- **THEN** the outcome is `DEGRADED` and the markup is `<i>✨ Generating rich content…</i>`

#### Scenario: Repairable attribute
- **WHEN** a block with an attribute not allowed on its element is rendered as final
- **THEN** the outcome is `REPAIRED` and the attribute is dropped

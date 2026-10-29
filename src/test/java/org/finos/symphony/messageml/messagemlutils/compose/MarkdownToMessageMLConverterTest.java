package org.finos.symphony.messageml.messagemlutils.compose;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MarkdownToMessageMLConverterTest {

    private static final Logger log = LoggerFactory.getLogger(MarkdownToMessageMLConverterTest.class);

    private MarkdownToMessageMLConverter converter;
    private MessageMLContext messageMLContext;

    @BeforeEach
    void setUp() {
        converter = new MarkdownToMessageMLConverter();
        messageMLContext = new MessageMLContext(new NoOpDataProvider());
    }

    /**
     * Validates that the generated MessageML can be parsed by Symphony's messageml-utils library.
     * This ensures the output is syntactically valid MessageML.
     *
     * @param messageML the generated MessageML to validate
     */
    private void assertValidMessageML(String messageML) {
        assertDoesNotThrow(() -> {
            messageMLContext.parseMessageML(messageML, null, null);
        }, "Generated MessageML should be parseable: " + messageML);

        assertNotNull(messageMLContext.getMessageML(), "Parsed MessageML tree should not be null");
        log.debug("Valid MessageML: {}", messageML);
    }

    @Test
    void convertTextFormattingAndSemantics_shouldSucceed() {
        String md = """
                # Hello Converter
                This **is** a _Markdown_ to *MessageML* converter.
                > By Thibault Pensec
                ## How does it work
                This is very simple, look at this `code` snippet:
                ```java
                public String convert(String md) {
                    return converter.convert(md);
                }
                ```
                
                Also & are managed!
                @[Jane Doe](jane.doe@example.com)
                #hashtag
                $AAPL
                """;

        String expectedMML = """
                <messageML><h1>Hello Converter</h1>This <b>is</b> a <i>Markdown</i> to <i>MessageML</i> converter.<br/><card><body>By Thibault Pensec</body></card><h2>How does it work</h2>This is very simple, look at this <span class="tempo-text-color--secondary">code</span> snippet:<br/><code>public String convert(String md) {
                    return converter.convert(md);
                }
                </code>Also &amp; are managed!<br/><mention email="jane.doe@example.com"/><br/><hash tag="hashtag"/><br/><cash tag="AAPL"/></messageML>""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);
        assertEquals(expectedMML, mml);
        // The mention no longer degrades to an anchor with a non-absolute URI, so the whole document
        // validates. Before the entity-layer fix this assertion was impossible and the test skipped
        // validation.
        assertValidMessageML(mml);
        log.info("Successfully converted Markdown to MessageML:\n{}", mml);
    }

    @Test
    void convertLinks_shouldProperlyFormatAnchorTags() {
        String md = "Check out [Symphony](https://symphony.com) for more info.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Check out <a href=\"https://symphony.com\">Symphony</a> for more info.</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertLinks_shouldEscapeSpecialCharactersInUrls() {
        String md = "[Search](https://example.com/search?q=test&sort=asc)";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><a href=\"https://example.com/search?q=test&amp;sort=asc\">Search</a></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertHashtags_shouldSupportDotsAndDashes() {
        String md = "#my-tag #version.1.0 #simple_tag";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><hash tag=\"my-tag\"/> <hash tag=\"version.1.0\"/> <hash tag=\"simple_tag\"/></messageML>", mml);
        assertValidMessageML(mml);
    }

    /**
     * Rewritten when the bare forms were tightened. This test previously asserted that {@code $msft},
     * {@code $Stock-123} and {@code $version.2} all became cashtags - the permissiveness that made every
     * monetary amount a mis-tagged instrument. A bare cashtag now requires an uppercase 2-5 character
     * ticker.
     */
    @Test
    void convertCashtags_bareFormRequiresAnUppercaseTicker() {
        String md = "$AAPL $msft $Stock-123 $version.2";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><cash tag=\"AAPL\"/> $msft $Stock-123 $version.2</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertIndentedCodeBlock_shouldWrapInCodeTags() {
        String md = """
                Here is some code:
                
                    int x = 42;
                    return x;
                
                End of code.""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Here is some code:<br/><code>int x = 42;\nreturn x;\n</code>End of code.</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertThematicBreak_shouldCreateHorizontalRule() {
        String md = """
                Section one
                
                ---
                
                Section two""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Section one<br/><hr/>Section two</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertImage_shouldCreateImgTag() {
        String md = "![Logo](https://example.com/logo.png)";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><img src=\"https://example.com/logo.png\"/></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertImage_shouldEscapeSpecialCharactersInSrc() {
        String md = "![Image](https://example.com/img?id=1&size=large)";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><img src=\"https://example.com/img?id=1&amp;size=large\"/></messageML>", mml);
        assertValidMessageML(mml);
    }

    /** Superseded by {@link #mentionSyntax_producesMentionElement()}, which asserts the same input. */
    @Test
    void convertMentions_shouldCreateMentionTags() {
        String md = "Hello @[John Smith](john.smith@company.com), please review this.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Hello <mention email=\"john.smith@company.com\"/>, please review this.</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertMentions_inPlainText_shouldCreateMentionTags() {
        String md = "`@[John](john@test.com)`";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><span class=\"tempo-text-color--secondary\">@[John](john@test.com)</span></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertOrderedList_shouldCreateOlTags() {
        String md = """
                1. First item
                2. Second item
                3. Third item""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><ol><li>First item</li><li>Second item</li><li>Third item</li></ol></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertBulletList_shouldCreateUlTags() {
        String md = """
                - Apple
                - Banana
                - Cherry""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><ul><li>Apple</li><li>Banana</li><li>Cherry</li></ul></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertBlockquote_shouldCreateCardTags() {
        String md = "> This is a quote";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><card><body>This is a quote</body></card></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertHeadings_shouldCreateHeadingTags() {
        String md = """
                # H1
                ## H2
                ### H3
                #### H4
                ##### H5
                ###### H6""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><h1>H1</h1><h2>H2</h2><h3>H3</h3><h4>H4</h4><h5>H5</h5><h6>H6</h6></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertSpecialCharacters_shouldEscapeHtml() {
        String md = "Use & and < symbols carefully.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Use &amp; and &lt; symbols carefully.</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertSpecialCharacters_inCode_shouldEscapeHtml() {
        String md = "Use `<tag>` and `&` in your code.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Use <span class=\"tempo-text-color--secondary\">&lt;tag&gt;</span> and <span class=\"tempo-text-color--secondary\">&amp;</span> in your code.</messageML>", mml);
        assertValidMessageML(mml);
    }

  /**
   * Without escaping, ${x} would be treated as a FreeMarker interpolation and throw during parsing
   */
  @Test
  void convertFreemarkerInterpolation_shouldEscapeDollarBrace() {
    String md = "The value ${x} is computed.";

    String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

    assertEquals("<messageML>The value &#36;{x} is computed.</messageML>", mml);
    assertValidMessageML(mml);
  }

  /**
   * Without escaping, #{x} would be treated as a FreeMarker directive and throw during parsing
   */
  @Test
  void convertFreemarkerDirective_shouldEscapeHashBrace() {
    String md = "Template uses #{expr} syntax.";

    String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

    assertEquals("<messageML>Template uses &#35;{expr} syntax.</messageML>", mml);
    assertValidMessageML(mml);
  }

  /**
   * LLM output containing LaTeX such as ${\text{...}} previously left an
   * unescaped ${...} that FreeMarker rejected with a parsing error
   */
  @Test
  void convertLatexWithDollarBraces_shouldNotBreakMessageML() {
    String md = "The set ${\\text{Red}} maps cleanly.";

    String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

    assertEquals("<messageML>The set &#36;{\\text{Red}} maps cleanly.</messageML>", mml);
    assertValidMessageML(mml);
  }

  @Test
    void convertTable_shouldCreateTableTags() {
        String md = """
                | Header 1 | Header 2 |
                |----------|----------|
                | Cell 1   | Cell 2   |
                | Cell 3   | Cell 4   |""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><table><thead><tr><th>Header 1</th><th>Header 2</th></tr></thead><tbody><tr><td>Cell 1</td><td>Cell 2</td></tr><tr><td>Cell 3</td><td>Cell 4</td></tr></tbody></table></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertTable_withFormattedContent_shouldPreserveFormatting() {
        String md = """
                | Feature | Status |
                |---------|--------|
                | **Bold** | _Italic_ |
                | `Code` | [Link](https://example.com) |""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><table><thead><tr><th>Feature</th><th>Status</th></tr></thead><tbody><tr><td><b>Bold</b></td><td><i>Italic</i></td></tr><tr><td><span class=\"tempo-text-color--secondary\">Code</span></td><td><a href=\"https://example.com\">Link</a></td></tr></tbody></table></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertTable_withSpecialCharacters_shouldEscapeHtml() {
        String md = """
                | Name | Value |
                |------|-------|
                | A & B | < 10 |""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><table><thead><tr><th>Name</th><th>Value</th></tr></thead><tbody><tr><td>A &amp; B</td><td>&lt; 10</td></tr></tbody></table></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertTable_singleColumn_shouldWork() {
        String md = """
                | Item |
                |------|
                | One  |
                | Two  |""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><table><thead><tr><th>Item</th></tr></thead><tbody><tr><td>One</td></tr><tr><td>Two</td></tr></tbody></table></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void convertTable_withSurroundingText_shouldWork() {
        String md = """
                Here is a table:
                
                | Col 1 | Col 2 |
                |-------|-------|
                | A     | B     |
                
                End of table.""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Here is a table:<br/><table><thead><tr><th>Col 1</th><th>Col 2</th></tr></thead><tbody><tr><td>A</td><td>B</td></tr></tbody></table>End of table.</messageML>", mml);
        assertValidMessageML(mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Entity layer.
    //
    // These tests began as a baseline pinning the defective behaviour so the fix had something to
    // turn green. They were rewritten - not deleted - when the fix landed.
    // ---------------------------------------------------------------------------------------------

    /**
     * Fixed: mention detection moved to the commonmark AST, because commonmark parses
     * {@code [label](destination)} into a {@code Link} before any {@code Text} node is visited - which
     * is why the old text-level regex was unreachable dead code.
     */
    @Test
    void mentionSyntax_producesMentionElement() {
        String md = "Hello @[John Smith](john.smith@company.com), please review this.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Hello <mention email=\"john.smith@company.com\"/>, please review this.</messageML>", mml);
        assertFalse(mml.contains("<a href="), "the mention must not degrade back to an anchor");
    }

    /**
     * Fixed: the output is now valid MessageML - previously the anchor carried a bare email as its
     * {@code href}, which is not an absolute URI, so Symphony rejected the whole message.
     */
    @Test
    void mentionSyntax_producesValidMessageML() {
        String md = "Hello @[John Smith](john.smith@company.com), please review this.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertValidMessageML(mml);
    }

    /** Fixed: the {@code mailto:} link form resolves to the same mention. */
    @Test
    void mailtoLinkForm_producesMentionElement() {
        String md = "Ping [Jane Doe](mailto:jane.doe@company.com)";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Ping <mention email=\"jane.doe@company.com\"/></messageML>", mml);
        assertValidMessageML(mml);
    }

    /**
     * Fixed: the uid form is now reachable. Measured against a live model, {@code sym:user/<id>} is
     * accepted only as *source* syntax - as an anchor {@code href} it fails Symphony's absolute-URI
     * rule, which is why it must resolve to the element.
     */
    @Test
    void symUserForm_producesUidMention() {
        String md = "Assigned to @[Ops Desk](sym:user/71811853189183).";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Assigned to <mention uid=\"71811853189183\"/>.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /**
     * A {@code sym:user/} destination that is not a user id resolves to nothing, so the link degrades
     * to its own text rather than to an anchor Symphony would reject.
     */
    @Test
    void symUserForm_withNonNumericId_degradesToText() {
        String md = "Assigned to @[Ops Desk](sym:user/not-an-id).";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Assigned to @Ops Desk.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** An ordinary documentation link is not a mention and keeps its anchor. */
    @Test
    void ordinaryLink_isNotAMention() {
        String md = "See [the docs](https://example.com/docs) for the schema.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>See <a href=\"https://example.com/docs\">the docs</a> for the schema.</messageML>", mml);
        assertFalse(mml.contains("<mention"));
        assertValidMessageML(mml);
    }

    /**
     * Fixed: a relative destination is not an absolute URI, so Symphony rejects the anchor and with
     * it the whole message. The link degrades to its own text, which keeps the sentence readable.
     *
     * <p>This is fixed here as well as in the validation pipeline on purpose: the pipeline repairs the
     * same fault for markup the converter did not produce, while fixing it at the source costs no
     * failed parse and no {@code validation_failure} metric on an input that is entirely ordinary.
     */
    @Test
    void relativeLinkDestination_degradesToText() {
        String md = "See [the runbook](/internal/runbook) for the steps.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>See the runbook for the steps.</messageML>", mml);
        assertFalse(mml.contains("<a href="), "an anchor with a relative href is rejected by Symphony");
        assertValidMessageML(mml);
    }

    /** A fragment and a bare file name are relative too. */
    @Test
    void fragmentAndBareFileDestinations_degradeToText() {
        String md = "See [above](#summary) and [the guide](guide.html).";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>See above and the guide.</messageML>", mml);
        assertValidMessageML(mml);
    }

    private static final String CANONICAL_MESSAGE_HREF = "https://open.symphony.com/?streamId=cGLj3wbzGI1lO7AcQM4Brn%2F%2F%2FmU6c3uEdA%3D%3D&amp;messageId=8bMFOfcc4WD%2BpJsS32IzWn%2F%2F%2Fl7koV4kbQ%3D%3D";
    private static final String RAW_MESSAGE_LINK = "https://open.symphony.com/?streamId=cGLj3wbzGI1lO7AcQM4Brn///mU6c3uEdA==&messageId=8bMFOfcc4WD+pJsS32IzWn///l7koV4kbQ==";

    /** A Symphony link the model built from raw base64 IDs is re-encoded before delivery. */
    @Test
    void symphonyLink_rawBase64Ids_areReEncoded() {
        String md = "See [the message](" + RAW_MESSAGE_LINK + ").";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>See <a href=\"" + CANONICAL_MESSAGE_HREF + "\">the message</a>.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** The forms the output-format contract shows, written with URL-safe unpadded IDs. */
    @Test
    void symphonyLink_contractForms_areCanonicalized() {
        String md = """
                <https://open.symphony.com/?streamId=cGLj3wbzGI1lO7AcQM4Brn___mU6c3uEdA&streamType=chatroom>

                <https://open.symphony.com/?streamId=cGLj3wbzGI1lO7AcQM4Brn___mU6c3uEdA&streamType=im>

                <https://open.symphony.com/?streamId=cGLj3wbzGI1lO7AcQM4Brn___mU6c3uEdA&messageId=8bMFOfcc4WD-pJsS32IzWn___l7koV4kbQ>""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        String room = "https://open.symphony.com/?streamId=cGLj3wbzGI1lO7AcQM4Brn%2F%2F%2FmU6c3uEdA%3D%3D&amp;streamType=";
        assertTrue(mml.contains("<a href=\"" + room + "chatroom\">" + room + "chatroom</a>"), mml);
        assertTrue(mml.contains("<a href=\"" + room + "im\">" + room + "im</a>"), mml);
        assertTrue(mml.contains("<a href=\"" + CANONICAL_MESSAGE_HREF + "\">" + CANONICAL_MESSAGE_HREF + "</a>"), mml);
        assertValidMessageML(mml);
    }

    /** Angle-bracket autolinks are {@code Link} nodes too. */
    @Test
    void symphonyLink_autolink_isReEncoded() {
        String md = "See <" + RAW_MESSAGE_LINK + ">";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.full());

        assertTrue(mml.contains("<a href=\"" + CANONICAL_MESSAGE_HREF + "\">"), mml);
        assertValidMessageML(mml);
    }

    /** The repair applies inside a card body and on streaming updates alike. */
    @Test
    void symphonyLink_insideCard_isReEncodedWhileStreaming() {
        String md = """
                :::card
                See [the message](%s).
                :::""".formatted(RAW_MESSAGE_LINK);

        String mml = converter.renderDocument(md, MessageMLRenderOptions.full().asIntermediate());

        assertTrue(mml.contains("<card><body>See <a href=\"" + CANONICAL_MESSAGE_HREF + "\">the message</a>.</body></card>"), mml);
        assertValidMessageML(mml);
    }

    /** A Symphony link that cannot open anything degrades to its text. */
    @Test
    void symphonyLink_unresolvable_degradesToText() {
        String md = "See [the message](https://open.symphony.com/?messageId=8bMFOfcc4WD-pJsS32IzWn___l7koV4kbQ).";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>See the message.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** Fixed: a monetary amount is not a financial instrument. */
    @Test
    void monetaryAmount_staysPlainText() {
        String md = "Revenue was $12 million this quarter, up from $9 million";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Revenue was $12 million this quarter, up from $9 million</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** Fixed: a decimal amount is not a cashtag either. */
    @Test
    void decimalAmount_staysPlainText() {
        String md = "The fee is $1.25 per contract.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>The fee is $1.25 per contract.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** Fixed: issue references are not topic tags. */
    @Test
    void issueReference_staysPlainText() {
        String md = "Fixed in #123, see also #456";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Fixed in #123, see also #456</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** Fixed: a version number is not a topic tag. */
    @Test
    void versionReference_staysPlainText() {
        String md = "Shipped in #2.1 of the connector.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Shipped in #2.1 of the connector.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /**
     * Fixed: sentence punctuation is no longer absorbed into the tag value. That defect produced
     * *valid* MessageML carrying a tag for an instrument or topic that does not exist, so no validation
     * gate would ever have caught it.
     */
    @Test
    void sentencePunctuation_terminatesTheTag() {
        String md = "Settled in $USD. Filed under #compliance.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Settled in <cash tag=\"USD\"/>. Filed under <hash tag=\"compliance\"/>.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** Punctuation outside the tag character class already terminated the tag and still does. */
    @Test
    void punctuationOutsideTheTagCharacterClass_terminatesTheTag() {
        String md = "Escalate $AAPL, then close #compliance!";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Escalate <cash tag=\"AAPL\"/>, then close <hash tag=\"compliance\"/>!</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** An interior dot or hyphen is still legal inside a tag. */
    @Test
    void interiorSeparators_remainPartOfTheTag() {
        String md = "See #version.1.0 and #my-tag";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>See <hash tag=\"version.1.0\"/> and <hash tag=\"my-tag\"/></messageML>", mml);
        assertValidMessageML(mml);
    }

    /** The forms the bare-form tightening must keep working. */
    @Test
    void legitimateBareTags_areStillTagged() {
        String md = "Compare $AAPL and $MSFT, filed under #compliance";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Compare <cash tag=\"AAPL\"/> and <cash tag=\"MSFT\"/>, filed under <hash tag=\"compliance\"/></messageML>", mml);
        assertValidMessageML(mml);
    }

    /**
     * Accepted residual of the bare-form rules: {@code $USD} satisfies the bare-ticker shape rule and
     * is tagged as an instrument. Pinned deliberately so a later "fix" cannot change it silently.
     */
    @Test
    void currencyCode_isTagged_acceptedResidual() {
        String md = "Settled in $USD";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Settled in <cash tag=\"USD\"/></messageML>", mml);
        assertValidMessageML(mml);
    }

    /**
     * Accepted residual of the bare-form rules: {@code #Q3} is tagged because the rule rejects only a
     * token whose *first* character is a digit. Pinned deliberately.
     */
    @Test
    void quarterToken_isTagged_acceptedResidual() {
        String md = "Results for #Q3 are final.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Results for <hash tag=\"Q3\"/> are final.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** A bare cashtag needs an uppercase 2-5 character ticker; anything else stays text. */
    @Test
    void bareCashtag_requiresATickerShape() {
        String md = "$msft $Stock-123 $version.2 $A $TOOLONGTICKER";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>$msft $Stock-123 $version.2 $A $TOOLONGTICKER</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void explicitEntityForms_areRecognisedWhenEnabled() {
        String md = "Position in $[AAPL], tracked as #[quarterly-review].";

        String mml = converter.renderDocument(md, new MessageMLRenderOptions(true, false, false, false));

        assertEquals("<messageML>Position in <cash tag=\"AAPL\"/>, tracked as <hash tag=\"quarterly-review\"/>.</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void explicitEntityForms_stayLiteralWhenDisabled() {
        String md = "Position in $[AAPL], tracked as #[quarterly-review].";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Position in $[AAPL], tracked as #[quarterly-review].</messageML>", mml);
        assertValidMessageML(mml);
    }

    /**
     * A tag value Symphony would reject is emitted as the literal text the model wrote, so the answer
     * survives even when the entity cannot be produced.
     */
    @Test
    void unrenderableTagValue_degradesToLiteralText() {
        String md = "Tagged as #[---] here.";

        String mml = converter.renderDocument(md, new MessageMLRenderOptions(true, false, false, false));

        assertEquals("<messageML>Tagged as #[---] here.</messageML>", mml);
        assertValidMessageML(mml);
    }

    /** Entity detection must never reach inside verbatim content. */
    @Test
    void entityDetection_doesNotApplyInsideCode() {
        String md = "Call `convert($AAPL, #compliance)` to reproduce.";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertFalse(mml.contains("<cash"), "no cashtag inside a code span");
        assertFalse(mml.contains("<hash"), "no hashtag inside a code span");
        assertValidMessageML(mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Native MessageML blocks: block detection fix and the flag-off literal fallback.
    // Native rendering itself (flag on) is covered by MarkdownToMessageMLConverterNativeBlocksTest.
    // ---------------------------------------------------------------------------------------------

    /** Defect fix: today's commonmark HtmlBlock handling silently dropped this instead of showing it. */
    @Test
    void midLineMessageMLTag_isNotABlockAndShowsAsLiteralText() {
        String md = "Some text <messageML> more text";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Some text &lt;messageML&gt; more text</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void nativeBlock_withFlagOff_rendersAsEscapedLiteralText() {
        String md = """
                Before.

                <messageML>
                <h1>Hello</h1>
                </messageML>

                After.""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Before.<br/>"
                + "&lt;messageML&gt;&lt;h1&gt;Hello&lt;/h1&gt;&lt;/messageML&gt;After.</messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void textAfterTheClosingTag_withFlagOff_isStillRenderedAsMarkdown() {
        String md = "<messageML><h1>Hello</h1></messageML> and **more**";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>&lt;messageML&gt;&lt;h1&gt;Hello&lt;/h1&gt;&lt;/messageML&gt;"
                + "and <b>more</b></messageML>", mml);
        assertValidMessageML(mml);
    }

    @Test
    void unterminatedNativeBlock_withFlagOff_rendersWithoutAClosingTag() {
        String md = """
                <messageML>
                <h1>Hello""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>&lt;messageML&gt;&lt;h1&gt;Hello</messageML>", mml);
        assertValidMessageML(mml);
    }
}

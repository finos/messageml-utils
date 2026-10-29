/*
 * Copyright 2026 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.finos.symphony.messageml.messagemlutils.compose;

import org.finos.symphony.messageml.messagemlutils.compose.parser.ContainerDirective;
import org.finos.symphony.messageml.messagemlutils.compose.parser.MessageMLBlock;
import org.finos.symphony.messageml.messagemlutils.compose.parser.SymphonyEntityInlineParser;
import org.finos.symphony.messageml.messagemlutils.compose.parser.SymphonyEntityNode;
import org.finos.symphony.messageml.messagemlutils.compose.parser.SymphonyMarkdownExtension;
import org.commonmark.Extension;
import org.commonmark.ext.gfm.tables.*;
import org.commonmark.node.*;
import org.commonmark.parser.Parser;

import java.util.List;
import java.util.regex.Pattern;

/**
 * Converts Markdown text into Symphony MessageML, including the Symphony-specific semantic entities
 * {@code <mention/>}, {@code <hash/>} and {@code <cash/>} and the {@code <card>} /
 * {@code <expandable-card>} content grouping elements.
 *
 * <p>The instance is immutable and safe to share across threads: {@link Parser} is thread-safe, a
 * fresh visitor is created per call and per-request configuration is passed as
 * {@link MessageMLRenderOptions} rather than held on the instance.
 *
 * <p>Two parsers are held on purpose. The main one knows the Symphony source syntax; the second one
 * does not and is used to parse the body of a container directive. That is what makes card nesting
 * impossible by construction rather than by a check that could be forgotten: an inner {@code :::card}
 * inside a card is simply not a directive to the parser that reads the body, so it comes back as
 * literal text.
 */
public class MarkdownToMessageMLConverter {

    private final Parser parser;
    private final Parser containerBodyParser;

    public MarkdownToMessageMLConverter() {
        List<Extension> extensions = List.of(TablesExtension.create(), SymphonyMarkdownExtension.create());
        this.parser = Parser.builder().extensions(extensions).build();
        this.containerBodyParser = Parser.builder()
                .extensions(List.of(TablesExtension.create(), new InlineOnlySymphonyExtension()))
                .build();
    }

    /** Registers the entity sigils but not the container directives - see the class javadoc. */
    private static final class InlineOnlySymphonyExtension implements Parser.ParserExtension {
        @Override
        public void extend(Parser.Builder parserBuilder) {
            parserBuilder.customInlineContentParserFactory(new SymphonyEntityInlineParser.Factory());
        }
    }

    /**
     * Converts Markdown to a MessageML <em>body</em> - the content without the enclosing
     * {@code <messageML>} element. This is the single conversion entry point; the BDK wraps a rootless
     * body when the message is sent and {@link #renderDocument} wraps it explicitly.
     *
     * @param markdown the Markdown to convert
     * @param options  per-request rendering configuration
     * @return the MessageML body, with no outer element
     */
    public String renderBody(String markdown, MessageMLRenderOptions options) {
        return render(markdown, options).body();
    }

    /**
     * Converts Markdown and reports which Symphony forms were produced, for callers that publish the
     * runtime form counters.
     */
    public RenderedMessageML render(String markdown, MessageMLRenderOptions options) {
        Node document = parser.parse(markdown == null ? "" : markdown);
        FormCounts.Tally tally = new FormCounts.Tally();
        MessageMLVisitor visitor = new MessageMLVisitor(options, tally, containerBodyParser);
        document.accept(visitor);
        return new RenderedMessageML(visitor.getMessageML(), tally.snapshot());
    }

    /**
     * Converts Markdown to a complete MessageML document.
     */
    public String renderDocument(String markdown, MessageMLRenderOptions options) {
        return "<messageML>" + renderBody(markdown, options) + "</messageML>";
    }

    static String escapeHtml(String input) {
        if (input == null) {
            return "";
        }
        return input.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#39;")
                .replace("${", "&#36;{")
                .replace("#{", "&#35;{");
    }

    /**
     * The main visitor class that traverses the Markdown AST (Abstract Syntax Tree)
     * and builds the MessageML string.
     */
    private static class MessageMLVisitor extends AbstractVisitor {
        private final StringBuilder sb = new StringBuilder();
        private final MessageMLRenderOptions options;
        private final FormCounts.Tally tally;
        private final Parser containerBodyParser;
        private final NativeMessageMLRenderer nativeRenderer = new NativeMessageMLRenderer();

        /** Conservative shape check for a mention destination that carries no explicit scheme. */
        private static final Pattern EMAIL = Pattern.compile("[^\\s@]+@[^\\s@]+\\.[^\\s@]+");

        private static final String MAILTO_SCHEME = "mailto:";
        private static final String SYM_USER_SCHEME = "sym:user/";
        private static final Pattern UID = Pattern.compile("\\d+");

        /** RFC 3986 scheme followed by a colon - what Symphony means by "absolute URI". */
        private static final Pattern ABSOLUTE_URI = Pattern.compile("(?i)[a-z][a-z0-9+.\\-]*:.*");

        /** Used when an {@code :::expand} carries no title: an empty header is a validation risk. */
        private static final String DEFAULT_EXPAND_HEADER = "Details";
        private static final String DEFAULT_EXPAND_STATE = "collapsed";

        MessageMLVisitor(MessageMLRenderOptions options, FormCounts.Tally tally,
                Parser containerBodyParser) {
            this.options = options;
            this.tally = tally;
            this.containerBodyParser = containerBodyParser;
        }

        public String getMessageML() {
            return sb.toString();
        }

        @Override
        public void visit(Text text) {
            sb.append(escapeHtml(text.getLiteral()));
            super.visit(text);
        }

        @Override
        public void visit(HardLineBreak hardLineBreak) {
            sb.append("<br/>");
            super.visit(hardLineBreak);
        }

        @Override
        public void visit(SoftLineBreak softLineBreak) {
            sb.append("<br/>");
            super.visit(softLineBreak);
        }

        @Override
        public void visit(Emphasis emphasis) {
            sb.append("<i>");
            visitChildren(emphasis);
            sb.append("</i>");
        }

        @Override
        public void visit(StrongEmphasis strongEmphasis) {
            sb.append("<b>");
            visitChildren(strongEmphasis);
            sb.append("</b>");
        }

        @Override
        public void visit(Link link) {
            String mention = mentionFor(link);
            if (mention != null) {
                // "@[Label](destination)" leaves the sigil as the last character of the preceding text
                // node, which the mention element replaces.
                dropTrailingAtSign();
                sb.append(mention);
                tally.mention();
                return;
            }
            // A Symphony deep link the model built itself is re-encoded into the canonical form, or
            // comes back null when it cannot open anything.
            String destination = SymphonyLinks.normalize(link.getDestination());
            if (isUnusableDestination(destination)) {
                // An anchor whose href is not an absolute URI is rejected by Symphony. Degrade to the
                // link's own text rather than emit a document that cannot be delivered.
                visitChildren(link);
                return;
            }
            sb.append("<a href=\"").append(escapeHtml(destination)).append("\">");
            if (isBareUrlText(link) && !destination.equals(link.getDestination())) {
                // An autolink shows its own URL: show the repaired one, not the one the model wrote.
                sb.append(escapeHtml(destination));
            } else {
                visitChildren(link);
            }
            sb.append("</a>");
        }

        /** Whether the link text is just the destination, as in {@code <https://...>}. */
        private static boolean isBareUrlText(Link link) {
            return link.getFirstChild() instanceof Text text
                    && link.getFirstChild() == link.getLastChild()
                    && text.getLiteral().equals(link.getDestination());
        }

        /**
         * Resolves the supported mention forms from the AST: an explicit {@code mailto:} or
         * {@code sym:user/} destination or a bare email address. Returns {@code null} when the link is
         * an ordinary link.
         */
        private static String mentionFor(Link link) {
            String destination = link.getDestination();
            if (destination == null || destination.isBlank()) {
                return null;
            }
            if (destination.startsWith(MAILTO_SCHEME)) {
                String email = destination.substring(MAILTO_SCHEME.length());
                return EMAIL.matcher(email).matches() ? "<mention email=\"" + escapeHtml(email) + "\"/>" : null;
            }
            if (destination.startsWith(SYM_USER_SCHEME)) {
                String uid = destination.substring(SYM_USER_SCHEME.length());
                return UID.matcher(uid).matches() ? "<mention uid=\"" + uid + "\"/>" : null;
            }
            if (EMAIL.matcher(destination).matches()) {
                return "<mention email=\"" + escapeHtml(destination) + "\"/>";
            }
            return null;
        }

        /**
         * A destination that is neither a resolvable mention nor a usable anchor target. Symphony
         * requires an absolute URI on {@code href}, so a scheme-less destination - a relative path,
         * a fragment, a bare file name - is emitted as text instead. The two mention
         * schemes are excluded as well: reaching here with one of them means the destination looked
         * like a mention but did not resolve and an anchor is not a sensible substitute.
         */
        private static boolean isUnusableDestination(String destination) {
            if (destination == null) {
                return true;
            }
            return destination.startsWith(SYM_USER_SCHEME)
                    || destination.startsWith(MAILTO_SCHEME)
                    || !ABSOLUTE_URI.matcher(destination).matches();
        }

        private void dropTrailingAtSign() {
            if (sb.length() > 0 && sb.charAt(sb.length() - 1) == '@') {
                sb.setLength(sb.length() - 1);
            }
        }

        @Override
        public void visit(Code code) {
            sb.append("<span class=\"tempo-text-color--secondary\">");
            sb.append(escapeHtml(code.getLiteral()));
            sb.append("</span>");
        }

        @Override
        public void visit(FencedCodeBlock fencedCodeBlock) {
            sb.append("<code>");
            sb.append(escapeHtml(fencedCodeBlock.getLiteral()));
            sb.append("</code>");
        }

        @Override
        public void visit(IndentedCodeBlock indentedCodeBlock) {
            sb.append("<code>");
            sb.append(escapeHtml(indentedCodeBlock.getLiteral()));
            sb.append("</code>");
        }

        @Override
        public void visit(ThematicBreak thematicBreak) {
            sb.append("<hr/>");
        }

        /**
         * Raw HTML that commonmark parsed on its own, outside a {@link MessageMLBlock} - a mid-line
         * {@code <messageML>} or any other literal tag the model wrote. Without this override
         * {@link AbstractVisitor}'s default (visit children) silently drops it, since these are leaf
         * nodes with no children; emitting the escaped source instead is the same fix the block parser
         * gives the top-level case.
         */
        @Override
        public void visit(HtmlBlock htmlBlock) {
            sb.append(escapeHtml(htmlBlock.getLiteral()));
        }

        @Override
        public void visit(HtmlInline htmlInline) {
            sb.append(escapeHtml(htmlInline.getLiteral()));
        }

        /**
         * Emitted for any destination. Verified against messageml-utils 1.5.0: the {@code Image}
         * element accepts a relative {@code src} as readily as an absolute one, unlike {@code href} on
         * an anchor, so no URL restriction is applied here. Whether the client resolves a relative
         * source usefully is a client question, not a validity one.
         */
        @Override
        public void visit(Image image) {
            sb.append("<img src=\"").append(escapeHtml(image.getDestination())).append("\"/>");
        }

        @Override
        public void visit(BulletList bulletList) {
            sb.append("<ul>");
            visitChildren(bulletList);
            sb.append("</ul>");
        }

        @Override
        public void visit(OrderedList orderedList) {
            sb.append("<ol>");
            visitChildren(orderedList);
            sb.append("</ol>");
        }

        @Override
        public void visit(ListItem listItem) {
            sb.append("<li>");
            visitChildren(listItem);
            sb.append("</li>");
        }

        /**
         * A Markdown blockquote has always silently become a Symphony card, which collides with
         * the explicit {@code :::card} syntax: two very different sources producing the same element.
         * The collision is resolved by the content-grouping toggle rather than by picking one - with
         * grouping off, every existing agent keeps the appearance it has today; with grouping on, a card
         * comes only from a directive and a quote is emphasised inline content.
         */
        @Override
        public void visit(BlockQuote blockQuote) {
            if (options.contentGrouping()) {
                sb.append("<i>");
                visitChildren(blockQuote);
                sb.append("</i>");
                return;
            }
            sb.append("<card><body>");
            visitChildren(blockQuote);
            sb.append("</body></card>");
            tally.card();
        }

        @Override
        public void visit(Heading heading) {
            sb.append(String.format("<h%d>", heading.getLevel()));
            visitChildren(heading);
            sb.append(String.format("</h%d>", heading.getLevel()));
        }

        @Override
        public void visit(Paragraph paragraph) {
            visitChildren(paragraph);
            if (paragraph.getNext() != null) {
                sb.append("<br/>");
            }
        }

        @Override
        public void visit(CustomBlock customBlock) {
            if (customBlock instanceof TableBlock) {
                sb.append("<table>");
                visitChildren(customBlock);
                sb.append("</table>");
            } else if (customBlock instanceof ContainerDirective directive) {
                renderContainer(directive);
            } else if (customBlock instanceof MessageMLBlock messageMLBlock) {
                renderMessageMLBlock(messageMLBlock);
            } else {
                super.visit(customBlock);
            }
        }

        /**
         * Shows a {@code <messageML>} block as the literal characters the model wrote when the flag is
         * off: a block is never silently dropped. With the flag on, the block instead
         * goes through {@link NativeMessageMLRenderer}'s sanitize/balance/validate/repair pipeline
         * and the result - real MessageML, the streaming placeholder or the
         * degraded card - is appended as is, since it is already well-formed and must not be escaped
         * again.
         */
        private void renderMessageMLBlock(MessageMLBlock block) {
            if (options.nativeMessageML()) {
                renderNativeBlock(block);
            } else {
                StringBuilder source = new StringBuilder("<messageML>").append(block.body());
                if (block.closed()) {
                    source.append("</messageML>");
                }
                sb.append(escapeHtml(source.toString()));
            }
            // Text the model wrote after the closing tag on the same line is not part of the block. It
            // is Markdown like everything around the block (dropping it would lose words).
            if (!block.trailingText().isEmpty()) {
                visitChildren(containerBodyParser.parse(block.trailingText()));
            }
        }

        private void renderNativeBlock(MessageMLBlock block) {
            NativeMessageMLRenderer.Result result = nativeRenderer.render(block.body(), options);
            sb.append(result.markup());
            // Unlike card()/expandable() below - which tally unconditionally and rely on the caller
            // never reading .forms() for an intermediate render - native
            // outcomes are filled by the renderer on the final render only, so this one is guarded at
            // the source rather than trusting every future caller to discard it correctly.
            if (!options.intermediate()) {
                switch (result.outcome()) {
                    case RENDERED -> tally.nativeRendered();
                    case REPAIRED -> tally.nativeRepaired();
                    case DEGRADED -> tally.nativeDegraded();
                }
            }
        }

        /**
         * Renders a {@code :::card} / {@code :::expand} container or puts it back as text.
         *
         * <p>With content grouping disabled the directive markers are emitted as literal text while the
         * body is still rendered as ordinary Markdown. Emitting the whole block as one escaped string
         * would have been simpler and would have thrown away the model's formatting, which is a worse
         * outcome than showing three colons.
         */
        private void renderContainer(ContainerDirective directive) {
            Node body = containerBodyParser.parse(directive.body());
            if (!options.contentGrouping()) {
                sb.append(escapeHtml(directive.openingLine())).append("<br/>");
                visitChildren(body);
                if (directive.closed()) {
                    sb.append("<br/>").append(escapeHtml(":::"));
                }
                return;
            }
            if (directive.kind() == ContainerDirective.Kind.CARD) {
                renderCard(directive, body);
            } else {
                renderExpandableCard(directive, body);
            }
        }

        /**
         * A leading heading becomes the {@code <header>}. {@code <header>} and {@code <body>}
         * must carry no attributes at all - {@code CardHeader} / {@code CardBody} enforce that with
         * {@code assertNoAttributes()} and mis-nesting fails on {@code assertParent}, so the nesting
         * this method emits has to be exact rather than merely plausible.
         */
        private void renderCard(ContainerDirective directive, Node body) {
            Node heading = body.getFirstChild() instanceof Heading first ? first : null;

            sb.append("<card");
            appendAttribute("accent", directive.accent());
            appendAttribute("iconSrc", directive.iconSrc());
            sb.append(">");

            if (heading != null) {
                sb.append("<header>");
                visitChildren(heading);
                sb.append("</header>");
            } else if (directive.title() != null) {
                sb.append("<header>").append(escapeHtml(directive.title())).append("</header>");
            }

            sb.append("<body>");
            visitSiblingsAfter(body, heading);
            sb.append("</body></card>");
            tally.card();
        }

        /**
         * {@code state} is required and must be one of three values - {@code ExpandableCard.validate()}
         * rejects a blank or unknown one - so it is always written, never omitted and never taken
         * verbatim from the model.
         *
         * <p>While streaming it is forced to {@code expanded}: a collapsed card renders as an
         * empty box, so a user watching a long answer arrive would see nothing at all until the final
         * update landed.
         */
        private void renderExpandableCard(ContainerDirective directive, Node body) {
            String state = options.intermediate()
                    ? "expanded"
                    : (directive.authoredState() == null ? DEFAULT_EXPAND_STATE : directive.authoredState());

            sb.append("<expandable-card state=\"").append(state).append("\">");
            sb.append("<header>")
                    .append(escapeHtml(directive.title() == null ? DEFAULT_EXPAND_HEADER : directive.title()))
                    .append("</header>");
            sb.append("<body");
            appendAttribute("variant", directive.bodyVariant());
            sb.append(">");
            visitChildren(body);
            sb.append("</body></expandable-card>");
            tally.expandable();
        }

        private void appendAttribute(String name, String value) {
            if (value != null) {
                sb.append(" ").append(name).append("=\"").append(escapeHtml(value)).append("\"");
            }
        }

        /** Visits every child of {@code parent} after {@code skip}, which may be {@code null}. */
        private void visitSiblingsAfter(Node parent, Node skip) {
            Node child = skip == null ? parent.getFirstChild() : skip.getNext();
            while (child != null) {
                Node next = child.getNext();
                child.accept(this);
                child = next;
            }
        }

        @Override
        public void visit(CustomNode customNode) {
            if (customNode instanceof SymphonyEntityNode entity) {
                renderEntity(entity);
            } else if (customNode instanceof TableHead) {
                sb.append("<thead>");
                visitChildren(customNode);
                sb.append("</thead>");
            } else if (customNode instanceof TableBody) {
                sb.append("<tbody>");
                visitChildren(customNode);
                sb.append("</tbody>");
            } else if (customNode instanceof TableRow) {
                sb.append("<tr>");
                visitChildren(customNode);
                sb.append("</tr>");
            } else if (customNode instanceof TableCell tableCell) {
                String tag = tableCell.isHeader() ? "th" : "td";
                sb.append("<").append(tag).append(">");
                visitChildren(customNode);
                sb.append("</").append(tag).append(">");
            } else {
                super.visit(customNode);
            }
        }

        /**
         * Emits the entity element or the characters the model wrote.
         *
         * <p>The bracketed forms are gated on the per-agent entity syntax: an agent that was never told
         * about them should not have {@code #[foo]} silently turn into a topic tag. The bare forms are
         * not gated, because a model emits {@code $AAPL} unprompted and the heuristics are there
         * to make that safe rather than to be switched off.
         */
        private void renderEntity(SymphonyEntityNode entity) {
            if (entity.explicit() && !options.entitySyntax()) {
                sb.append(escapeHtml(entity.source()));
                return;
            }
            sb.append("<").append(entity.kind().element())
                    .append(" tag=\"").append(entity.value()).append("\"/>");
            if (entity.kind() == SymphonyEntityNode.Kind.HASH) {
                tally.hash();
            } else {
                tally.cash();
            }
        }
    }
}

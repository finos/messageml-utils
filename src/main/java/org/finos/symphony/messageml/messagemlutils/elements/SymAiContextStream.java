
package org.finos.symphony.messageml.messagemlutils.elements;

import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.commonmark.node.Node;

/**
 * Represents the {@code <sym-ai-stream>} element in MessageML.
 *
 * <p>This element references a Symphony conversation stream within a {@link SymAiContext}.
 * It allows AI features to access and process messages from specific streams.</p>
 *
 * <h3>Attributes</h3>
 * <ul>
 *   <li>{@code id} (required) - The unique identifier of the stream</li>
 *   <li>{@code from} (optional) - Timestamp (in milliseconds since epoch) defining the start
 *       of the time range to consider</li>
 *   <li>{@code to} (optional) - Timestamp (in milliseconds since epoch) defining the end
 *       of the time range to consider</li>
 * </ul>
 *
 * <h3>Example Usage</h3>
 * <pre>{@code
 * <sym-ai-stream id="KdmVxzr6mAid5DrITQSoln___mP_3JRndA"/>
 * <sym-ai-stream id="streamId123" from="1707235200000" to="1707321600000"/>
 * }</pre>
 *
 * <p>This element must be a child of {@link SymAiContext} and cannot contain any content.</p>
 *
 * @see SymAiContext
 */
public class SymAiContextStream extends Element {
    /** The MessageML tag name for this element. */
    public static final String MESSAGEML_TAG = "sym-ai-stream";
    private static final String ATTR_ID = "id";
    private static final String ATTR_FROM = "from";
    private static final String ATTR_TO = "to";

    /**
     * Constructs a new SymAiContextStream element.
     *
     * @param parent the parent element in the MessageML tree
     */
    public SymAiContextStream(Element parent) {
        super(parent, MESSAGEML_TAG);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void buildAttribute(MessageMLParser parser, org.w3c.dom.Node item) throws InvalidInputException {
        switch (item.getNodeName()) {
            case ATTR_ID:
                setAttribute(ATTR_ID, getStringAttribute(item));
                break;
            case ATTR_FROM:
                setAttribute(ATTR_FROM, getStringAttribute(item));
                break;
            case ATTR_TO:
                setAttribute(ATTR_TO, getStringAttribute(item));
                break;
            default:
                super.buildAttribute(parser, item);
        }
    }

    /**
     * Validates the SymAiContextStream element.
     *
     * <p>This method ensures that:</p>
     * <ul>
     *   <li>The element has no content (text or child elements)</li>
     *   <li>The required {@code id} attribute is present</li>
     * </ul>
     *
     * @throws InvalidInputException if validation fails
     */
    @Override
    public void validate() throws InvalidInputException {
        assertNoContent();
        if (getAttribute(ATTR_ID) == null) {
            throw new InvalidInputException("The attribute 'id' is required for the element 'sym-ai-stream'.");
        }
    }

    /**
     * Returns the Markdown representation of this element.
     *
     * <p>This element has no Markdown representation and returns {@code null}.</p>
     *
     * @return {@code null} as this element has no Markdown output
     */
    @Override
    public Node asMarkdown() {
        return null;
    }
}

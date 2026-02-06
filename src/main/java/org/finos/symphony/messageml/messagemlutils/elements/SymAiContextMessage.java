
package org.finos.symphony.messageml.messagemlutils.elements;

import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.commonmark.node.Node;

/**
 * Represents the {@code <sym-ai-message>} element in MessageML.
 *
 * <p>This element references a specific Symphony message within a {@link SymAiContext}.
 * It allows AI features to access and process individual messages by their unique identifier.</p>
 *
 * <h3>Attributes</h3>
 * <ul>
 *   <li>{@code id} (required) - The unique identifier of the message</li>
 * </ul>
 *
 * <h3>Example Usage</h3>
 * <pre>{@code
 * <sym-ai-message id="msgId456"/>
 * }</pre>
 *
 * <p>This element must be a child of {@link SymAiContext} and cannot contain any content.</p>
 *
 * @see SymAiContext
 */
public class SymAiContextMessage extends Element {
    /** The MessageML tag name for this element. */
    public static final String MESSAGEML_TAG = "sym-ai-message";
    private static final String ATTR_ID = "id";

    /**
     * Constructs a new SymAiContextMessage element.
     *
     * @param parent the parent element in the MessageML tree
     */
    public SymAiContextMessage(Element parent) {
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
            default:
                super.buildAttribute(parser, item);
        }
    }

    /**
     * Validates the SymAiContextMessage element.
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
            throw new InvalidInputException("The attribute 'id' is required for the element 'sym-ai-message'.");
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


package org.finos.symphony.messageml.messagemlutils.elements;

import org.commonmark.node.Text;
import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.commonmark.node.Node;

/**
 * Represents the {@code <sym-ai-attachment>} element in MessageML.
 *
 * <p>This element references a file attachment within a {@link SymAiContext}.
 * It allows AI features to access and process attachments by specifying the stream,
 * message, and file identifiers.</p>
 *
 * <h3>Attributes</h3>
 * <ul>
 *   <li>{@code streamId} (optional) - The identifier of the stream containing the attachment</li>
 *   <li>{@code messageId} (optional) - The identifier of the message containing the attachment</li>
 *   <li>{@code fileId} (optional) - The unique identifier of the file attachment</li>
 * </ul>
 *
 * <h3>Example Usage</h3>
 * <pre>{@code
 * <sym-ai-attachment streamId="streamId123" messageId="msgId456" fileId="fileId789"/>
 * }</pre>
 *
 * <p>This element must be a child of {@link SymAiContext} and cannot contain any content.</p>
 *
 * @see SymAiContext
 */
public class SymAiContextAttachment extends Element {
    /** The MessageML tag name for this element. */
    public static final String MESSAGEML_TAG = "sym-ai-attachment";
    private static final String ATTR_STREAM_ID = "streamId";
    private static final String ATTR_MESSAGE_ID = "messageId";
    private static final String ATTR_FILE_ID = "fileId";

    /**
     * Constructs a new SymAiContextAttachment element.
     *
     * @param parent the parent element in the MessageML tree
     */
    public SymAiContextAttachment(Element parent) {
        super(parent, MESSAGEML_TAG);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void buildAttribute(MessageMLParser parser, org.w3c.dom.Node item) throws InvalidInputException {
        switch (item.getNodeName()) {
            case ATTR_STREAM_ID:
            case ATTR_MESSAGE_ID:
            case ATTR_FILE_ID:
                setAttribute(item.getNodeName(), getStringAttribute(item));
                break;
            default:
                super.buildAttribute(parser, item);
        }
    }

    /**
     * Validates the SymAiContextAttachment element.
     *
     * <p>This method ensures that the element has no content (text or child elements).</p>
     *
     * @throws InvalidInputException if validation fails
     */
    @Override
    public void validate() throws InvalidInputException {
        assertNoContent();
    }

    /**
     * Returns the Markdown representation of this element.
     *
     * <p>Generates a text representation showing the attachment identifiers in the format:
     * {@code Attachment(streamId=..., messageId=..., fileId=...)}</p>
     *
     * @return a Text node containing the attachment information
     */
    @Override
    public Node asMarkdown() {
        return new Text(String.format("Attachment(streamdId=%s, messageId=%s, fileId=%s)", getAttribute(ATTR_STREAM_ID), getAttribute(ATTR_MESSAGE_ID), getAttribute(ATTR_FILE_ID)));
    }
}

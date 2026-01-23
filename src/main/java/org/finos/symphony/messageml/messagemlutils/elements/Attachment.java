
package org.finos.symphony.messageml.messagemlutils.elements;

import org.commonmark.node.Paragraph;
import org.commonmark.node.Text;
import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.commonmark.node.Node;

public class Attachment extends Element {
    public static final String MESSAGEML_TAG = "sym-ai-attachment";
    private static final String ATTR_STREAM_ID = "streamId";
    private static final String ATTR_MESSAGE_ID = "messageId";
    private static final String ATTR_FILE_ID = "fileId";

    public Attachment(Element parent) {
        super(parent, MESSAGEML_TAG);
    }

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

    @Override
    public void validate() throws InvalidInputException {
        assertNoContent();
    }

    @Override
    public Node asMarkdown() {
        return new Text(String.format("Attachment(streamdId=%s, messageId=%s, fileId=%s)", getAttribute(ATTR_STREAM_ID), getAttribute(ATTR_MESSAGE_ID), getAttribute(ATTR_FILE_ID)));
    }
}

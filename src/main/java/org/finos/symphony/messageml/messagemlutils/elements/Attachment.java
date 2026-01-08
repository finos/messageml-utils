
package org.finos.symphony.messageml.messagemlutils.elements;

import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.w3c.dom.Node;

public class Attachment extends Element {
    public static final String MESSAGEML_TAG = "attachment";
    private static final String ATTR_STREAM_ID = "streamId";
    private static final String ATTR_MESSAGE_ID = "messageId";
    private static final String ATTR_FILE_ID = "fileId";

    public Attachment(Element parent) {
        super(parent, MESSAGEML_TAG);
    }

    @Override
    protected void buildAttribute(MessageMLParser parser, Node item) throws InvalidInputException {
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
}

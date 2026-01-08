
package org.finos.symphony.messageml.messagemlutils.elements;

import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.w3c.dom.Node;

public class Message extends Element {
    public static final String MESSAGEML_TAG = "message";
    private static final String ATTR_ID = "id";

    public Message(Element parent) {
        super(parent, MESSAGEML_TAG);
    }

    @Override
    protected void buildAttribute(MessageMLParser parser, Node item) throws InvalidInputException {
        switch (item.getNodeName()) {
            case ATTR_ID:
                setAttribute(ATTR_ID, getStringAttribute(item));
                break;
            default:
                super.buildAttribute(parser, item);
        }
    }

    @Override
    public void validate() throws InvalidInputException {
        assertNoContent();
        if (getAttribute(ATTR_ID) == null) {
            throw new InvalidInputException("The attribute 'id' is required for the element 'message'.");
        }
    }
}

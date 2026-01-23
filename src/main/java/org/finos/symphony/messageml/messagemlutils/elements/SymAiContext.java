
package org.finos.symphony.messageml.messagemlutils.elements;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.commonmark.node.Node;
import org.commonmark.node.Paragraph;

import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;

import java.util.Arrays;
import java.util.List;

public class SymAiContext extends Entity {
    public static final String MESSAGEML_TAG = "sym-ai-context";
    public static final String ENTITY_TYPE = "com.symphony.ai.context";
    private static final String ENTITY_VERSION = "1.0";
    private static final String ENTITY_SUBTYPE = "com.symphony.ai.contextId";

    private static final List<String> ALLOWED_CHILDREN = Arrays.asList(Stream.MESSAGEML_TAG, Message.MESSAGEML_TAG, Attachment.MESSAGEML_TAG);

    public SymAiContext(Element parent, int entityIndex) {
        super(parent, MESSAGEML_TAG, Entity.DEFAULT_PRESENTATIONML_TAG, FormatEnum.MESSAGEML);
        this.entityId = getEntityId(entityIndex);
    }

    @Override
    public void validate() throws InvalidInputException {
        // Check if root MessageML has beta="true"
        MessageML root = getRoot();
        if (root == null || !root.isBeta()) {
            throw new InvalidInputException(
                    "Element \"sym-ai-context\" is only allowed when messageML has beta=\"true\"");
        }

        super.validate();
        assertNoText();

        for (Element child : getChildren()) {
            if (!ALLOWED_CHILDREN.contains(child.getMessageMLTag())) {
                throw new InvalidInputException(
                        "Element '" + child.getMessageMLTag() + "' is not allowed in '" + MESSAGEML_TAG + "'. "
                                + "Allowed elements are: " + ALLOWED_CHILDREN);
            }
        }
    }

    /**
     * Traverse up the parent chain to find the root MessageML element.
     */
    private MessageML getRoot() {
        Element current = this;
        while (current.getParent() != null) {
            current = current.getParent();
        }
        return (current instanceof MessageML) ? (MessageML) current : null;
    }

    @Override
    public ObjectNode asEntityJson(ObjectNode parent) {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        node.put(TYPE_FIELD, getEntityType());
        node.put(VERSION_FIELD, getEntityVersion());

        ArrayNode messageNode = node.putArray("messages");
        ArrayNode streamNode = node.putArray("streams");
        ArrayNode attachmentNode = node.putArray("attachments");

        for(Element child: getChildren()) {
            switch (child.getMessageMLTag()) {
                case Message.MESSAGEML_TAG:
                    ObjectNode messageObj = new ObjectNode(JsonNodeFactory.instance);
                    messageObj.put("id", child.getAttribute("id"));
                    messageNode.add(messageObj);
                    break;
                case Stream.MESSAGEML_TAG:
                    ObjectNode streamObj = new ObjectNode(JsonNodeFactory.instance);
                    streamObj.put("id", child.getAttribute("id"));
                    streamNode.add(streamObj);
                    break;
                case Attachment.MESSAGEML_TAG:
                    ObjectNode attachmentObj = new ObjectNode(JsonNodeFactory.instance);
                    attachmentObj.put("id", child.getAttribute("fileId"));
                    attachmentObj.put("streamId", child.getAttribute("streamId"));
                    attachmentObj.put("messageId", child.getAttribute("messageId"));
                    attachmentNode.add(attachmentObj);
                    break;
            }
        }

        parent.set(entityId, node);
        return node;
    }

    @Override
    public void asPresentationML(XmlPrintStream out,
        org.finos.symphony.messageml.messagemlutils.MessageMLContext context) {
        out.printElement(presentationMLTag, asText(), CLASS_ATTR, PRESENTATIONML_CLASS,
            ENTITY_ID_ATTR, entityId);
    }

    @Override
    public String asText() {
        return "";
    }

    @Override
    protected String getEntityValue() {
        // This will likely need to be a unique ID for the context
        return getEntityId(0);
    }

    @Override
    protected String getEntitySubType() {
        return ENTITY_SUBTYPE;
    }

    @Override
    protected String getEntityVersion() {
        return ENTITY_VERSION;
    }

    @Override
    protected String getEntityType() {
        return ENTITY_TYPE;
    }

    @Override
    protected String getEntityIdPrefix() {
        return MESSAGEML_TAG;
    }
}

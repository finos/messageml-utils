
package org.finos.symphony.messageml.messagemlutils.elements;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;

import java.util.Arrays;
import java.util.List;

/**
 * Represents the {@code <sym-ai-context>} element in MessageML.
 *
 * <p>This element provides context information for Symphony AI features by grouping references
 * to streams, messages, and attachments. It is a beta feature and can only be used when the
 * root {@code <messageML>} element has the {@code beta="true"} attribute set.</p>
 *
 * <h3>Allowed Children</h3>
 * <ul>
 *   <li>{@link SymAiContextStream} - References to conversation streams</li>
 *   <li>{@link SymAiContextMessage} - References to specific messages</li>
 *   <li>{@link SymAiContextAttachment} - References to file attachments</li>
 * </ul>
 *
 * <h3>Example Usage</h3>
 * <pre>{@code
 * <messageML beta="true">
 *   <sym-ai-context>
 *     <sym-ai-stream id="streamId123" from="1707235200000" to="1707321600000"/>
 *     <sym-ai-message id="msgId456"/>
 *     <sym-ai-attachment streamId="streamId123" messageId="msgId456" fileId="fileId789"/>
 *   </sym-ai-context>
 *   Please summarize the conversation.
 * </messageML>
 * }</pre>
 *
 * <h3>Entity JSON Output</h3>
 * <p>The element generates an entity JSON structure with arrays for messages, streams, and attachments:</p>
 * <pre>{@code
 * {
 *   "sym-ai-context1": {
 *     "type": "com.symphony.ai.context",
 *     "version": "1.0",
 *     "messages": [{"id": "msgId456"}],
 *     "streams": [{"id": "streamId123", "from": "1707235200000", "to": "1707321600000"}],
 *     "attachments": [{"id": "fileId789", "streamId": "streamId123", "messageId": "msgId456"}]
 *   }
 * }
 * }</pre>
 *
 * @see SymAiContextStream
 * @see SymAiContextMessage
 * @see SymAiContextAttachment
 */
public class SymAiContext extends Entity {
    public static final String MESSAGEML_TAG = "sym-ai-context";
    public static final String ENTITY_TYPE = "com.symphony.ai.context";
    private static final String ENTITY_VERSION = "1.0";
    private static final String ENTITY_SUBTYPE = "com.symphony.ai.contextId";

    private static final List<String> ALLOWED_CHILDREN = Arrays.asList(SymAiContextStream.MESSAGEML_TAG, SymAiContextMessage.MESSAGEML_TAG, SymAiContextAttachment.MESSAGEML_TAG);

    /**
     * Constructs a new SymAiContext element.
     *
     * @param parent      the parent element in the MessageML tree
     * @param entityIndex the index used to generate a unique entity ID
     */
    public SymAiContext(Element parent, int entityIndex) {
        super(parent, MESSAGEML_TAG, Entity.DEFAULT_PRESENTATIONML_TAG, FormatEnum.MESSAGEML);
        this.entityId = getEntityId(entityIndex);
    }

    /**
     * Validates the SymAiContext element.
     *
     * <p>This method checks that:</p>
     * <ul>
     *   <li>The root {@code <messageML>} element has {@code beta="true"}</li>
     *   <li>The element contains no direct text content</li>
     *   <li>All child elements are one of the allowed types: {@code <sym-ai-stream>},
     *       {@code <sym-ai-message>}, or {@code <sym-ai-attachment>}</li>
     * </ul>
     *
     * @throws InvalidInputException if validation fails
     */
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

    /**
     * Generates the entity JSON representation for this context element.
     *
     * <p>The generated JSON contains arrays for messages, streams, and attachments,
     * populated from the child elements of this context.</p>
     *
     * @param parent the parent JSON object to add this entity to
     * @return the JSON node representing this entity
     */
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
                case SymAiContextMessage.MESSAGEML_TAG:
                    ObjectNode messageObj = new ObjectNode(JsonNodeFactory.instance);
                    messageObj.put("id", child.getAttribute("id"));
                    messageNode.add(messageObj);
                    break;
                case SymAiContextStream.MESSAGEML_TAG:
                    ObjectNode streamObj = new ObjectNode(JsonNodeFactory.instance);
                    streamObj.put("id", child.getAttribute("id"));
                    if (child.getAttribute("from") != null) {
                        streamObj.put("from", child.getAttribute("from"));
                    }
                    if (child.getAttribute("to") != null) {
                        streamObj.put("to", child.getAttribute("to"));
                    }
                    streamNode.add(streamObj);
                    break;
                case SymAiContextAttachment.MESSAGEML_TAG:
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

package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * This class specify the Symphony Component UIAction which is represented by the tag name "ui-action".
 * The messageML representation of this element can contain the following fields:
 * <ul>
 *   <li>trigger  -> default 'click', trigger of the action. For now only 'click' is supported</li>
 *   <li>action (required) -> action that can be executed on trigger. For now only 'open-im' is supported</li>
 * </ul>
 * The open-im action can support the following attributes:
 * <ul>
 *   <li>user-ids (exclusive with stream-id)-> list of the users we want the action to be applied on</li>
 *   <li>stream-id (exclusive with user-ids) -> stream we want the action to be applied on</li>
 *   <li>side-by-side -> default true, open the result chat in a new module and keeps the parent one open side by side.
 *                     If false, the parent one will be replaced.</li>
 * <ul/>
 * For now only Buttons and other UIActions will be allowed as child of a ui-action.
 */

public class UIAction extends Element {

  public static final String MESSAGEML_TAG = "ui-action";

  private static final String TRIGGER_ATTR = "trigger";
  private static final String ACTION_ATTR = "action";
  private static final String USER_IDS_ATTR = "user-ids";
  private static final String STREAM_ID_ATTR = "stream-id";
  private static final String SIDE_BY_SIDE_ATTR = "side-by-side";

  private static final String DEFAULT_TRIGGER = "click";
  private static final String ALLOWED_ACTION = "open-im";
  private static final int MAX_USER_IDS = 15;

  private static final String PRESENTATIONML_TAG = "div";
  private static final String PRESENTATIONML_ACTION_ATTR = "data-action";
  private static final String PRESENTATIONML_TRIGGER_ATTR = "data-trigger";
  private static final String PRESENTATIONML_USER_IDS_ATTR = "data-user-ids";
  private static final String PRESENTATIONML_STREAM_ID_ATTR = "data-stream-id";
  private static final String PRESENTATIONML_SIDE_BY_SIDE_ATTR = "data-side-by-side";


  public UIAction(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
    setAttribute(TRIGGER_ATTR, DEFAULT_TRIGGER);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser, org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ACTION_ATTR:
      case TRIGGER_ATTR:
      case USER_IDS_ATTR:
      case STREAM_ID_ATTR:
      case SIDE_BY_SIDE_ATTR:
        if (this.format != FormatEnum.MESSAGEML) {
          throwInvalidInputException(item);
        }
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    assertUIActionAllowedChildren();

    assertAttributeNotBlank(ACTION_ATTR);
    assertAttributeValue(ACTION_ATTR, Collections.singleton(ALLOWED_ACTION));
    // validate attributes specific to the 'open-im' action
    validateOpenChatActionAttributes();

    if(getAttribute(TRIGGER_ATTR) != null) {
      assertAttributeValue(TRIGGER_ATTR, Collections.singleton(DEFAULT_TRIGGER));
    }
  }

  private void validateOpenChatActionAttributes() throws InvalidInputException {
    if(getAttribute(SIDE_BY_SIDE_ATTR) != null) {
      assertAttributeIsBoolean(SIDE_BY_SIDE_ATTR);
    }
    if (getAttribute(USER_IDS_ATTR) != null && getAttribute(STREAM_ID_ATTR) != null) {
      throw new InvalidInputException("Only one between \"stream-id\" and \"user-ids\" is allowed");
    }
    if (getAttribute(USER_IDS_ATTR) == null && getAttribute(STREAM_ID_ATTR) == null) {
      throw new InvalidInputException("At least one between \"stream-id\" and \"user-ids\" should be present");
    }
    if (getAttribute(USER_IDS_ATTR) != null) {
      validateUserIdsAttribute();
    }
  }

  private void assertUIActionAllowedChildren() throws InvalidInputException {
    List<Class<? extends Element>> allowedChildren = Arrays.asList(Button.class, UIAction.class);
    assertContentModel(allowedChildren);
    assertContainsAlwaysChildOfType(allowedChildren);
  }

  @Override
  void asPresentationML(XmlPrintStream out, MessageMLContext context) {
    Map<String, String> presentationAttrs = buildAUIActionAttributes();
    out.openElement(getPresentationMLTag(), presentationAttrs);
    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }
    out.closeElement();
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }

  private void validateUserIdsAttribute() throws InvalidInputException {
    String userIds = getAttribute(USER_IDS_ATTR);
    try {
      List<Long> userIdsList = MAPPER.readValue(userIds, MAPPER.getTypeFactory().constructCollectionType(List.class, Long.class));
      if(userIdsList.size() > MAX_USER_IDS){
        throw new InvalidInputException("Attribute \"user-ids\" contains more values than allowed. Max value is " + MAX_USER_IDS);
      }
    } catch (JsonProcessingException e) {
      throw new InvalidInputException("Attribute \"user-ids\" contains an unsupported format, should be an array of user ids");
    }
  }

  private Map<String, String> buildAUIActionAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(CLASS_ATTR, MESSAGEML_TAG);
    presentationAttrs.put(PRESENTATIONML_ACTION_ATTR, getAttribute(ACTION_ATTR));

    if (getAttribute(TRIGGER_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_TRIGGER_ATTR, getAttribute(TRIGGER_ATTR));
    }
    if (getAttribute(USER_IDS_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_USER_IDS_ATTR, getAttribute(USER_IDS_ATTR));
    }
    if (getAttribute(STREAM_ID_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_STREAM_ID_ATTR, getAttribute(STREAM_ID_ATTR));
    }
    if (getAttribute(SIDE_BY_SIDE_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_SIDE_BY_SIDE_ATTR, getAttribute(SIDE_BY_SIDE_ATTR));
    }

    return presentationAttrs;
  }


}

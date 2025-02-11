package org.finos.symphony.messageml.messagemlutils.elements;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;

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
  public static final String TARGET_ID = "target-id";

  private static final String DEFAULT_TRIGGER = "click";
  private static final String OPEN_IM = "open-im";
  private static final String OPEN_DIALOG = "open-dialog";
  private static final List<String> ALLOWED_ACTIONS = Arrays.asList(OPEN_IM, OPEN_DIALOG);
  private static final int MAX_USER_IDS = 15;

  private static final String PRESENTATIONML_TAG = "div";
  public static final String PRESENTATIONML_CLASS = MESSAGEML_TAG;
  private static final String PRESENTATIONML_ACTION_ATTR = "data-action";
  private static final String PRESENTATIONML_TRIGGER_ATTR = "data-trigger";
  private static final String PRESENTATIONML_USER_IDS_ATTR = "data-user-ids";
  private static final String PRESENTATIONML_STREAM_ID_ATTR = "data-stream-id";
  private static final String PRESENTATIONML_SIDE_BY_SIDE_ATTR = "data-side-by-side";
  private static final String PRESENTATIONML_DATA_TARGET_ID_ATTR = "data-target-id";

  public UIAction(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
    setAttribute(TRIGGER_ATTR, DEFAULT_TRIGGER);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser, org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ACTION_ATTR:
      case PRESENTATIONML_ACTION_ATTR:
        setAttribute(ACTION_ATTR, getStringAttribute(item));
        break;
      case TRIGGER_ATTR:
      case PRESENTATIONML_TRIGGER_ATTR:
        setAttribute(TRIGGER_ATTR, getStringAttribute(item));
        break;
      case USER_IDS_ATTR:
      case PRESENTATIONML_USER_IDS_ATTR:
        setAttribute(USER_IDS_ATTR, getStringAttribute(item));
        break;
      case STREAM_ID_ATTR:
      case PRESENTATIONML_STREAM_ID_ATTR:
        setAttribute(STREAM_ID_ATTR, getStringAttribute(item));
        break;
      case SIDE_BY_SIDE_ATTR:
      case PRESENTATIONML_SIDE_BY_SIDE_ATTR:
        setAttribute(SIDE_BY_SIDE_ATTR, getStringAttribute(item));
        break;
      case TARGET_ID:
      case PRESENTATIONML_DATA_TARGET_ID_ATTR:
        setAttribute(TARGET_ID, getStringAttribute(item));
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    assertUIActionAllowedChildren();

    assertAttributeNotBlank(ACTION_ATTR);
    assertAttributeValue(ACTION_ATTR, ALLOWED_ACTIONS);

    final String actionAttribute = getAttribute(ACTION_ATTR);
    if (actionAttribute.equals(OPEN_IM)) {
      validateOpenChatActionAttributes();
    } else if (actionAttribute.equals(OPEN_DIALOG)) {
      validateIdAttribute(TARGET_ID);
    }

    if (getAttribute(TRIGGER_ATTR) != null) {
      assertAttributeValue(TRIGGER_ATTR, Collections.singleton(DEFAULT_TRIGGER));
    }
  }

  private void validateOpenChatActionAttributes() throws InvalidInputException {
    if (getAttribute(SIDE_BY_SIDE_ATTR) != null) {
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

  @Override
  void updateBiContext(BiContext context) {
    super.updateBiContext(context);

    final String actionAttribute = getAttribute(ACTION_ATTR);
    if (actionAttribute != null) {
      switch (actionAttribute) {
        case OPEN_IM:
          context.updateItemCount(BiFields.OPENIM.getValue());
          break;
        case OPEN_DIALOG:
          context.updateItemCount(BiFields.OPENDIALOG.getValue());
          break;
      }
    }
  }

  private void validateUserIdsAttribute() throws InvalidInputException {
    String userIds = getAttribute(USER_IDS_ATTR);
    try {
      List<Long> userIdsList =
          MAPPER.readValue(userIds, MAPPER.getTypeFactory().constructCollectionType(List.class, Long.class));
      if (userIdsList.size() > MAX_USER_IDS) {
        throw new InvalidInputException(
            "Attribute \"user-ids\" contains more values than allowed. Max value is " + MAX_USER_IDS);
      }
    } catch (JsonProcessingException e) {
      throw new InvalidInputException(
          "Attribute \"user-ids\" contains an unsupported format, should be an array of user ids");
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
    if (getAttribute(TARGET_ID) != null) {
      presentationAttrs.put(PRESENTATIONML_DATA_TARGET_ID_ATTR, getAttribute(TARGET_ID));
    }

    return presentationAttrs;
  }

}

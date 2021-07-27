package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class UIActionTest extends ElementTest {

  @Test
  public void testUIActionWithInvalidAction() throws InvalidInputException, IOException, ProcessingException {
    String inputMessageML =
        "<messageML><ui-action action=\"open\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"action\" of element \"ui-action\" can only be one of the "
        + "following values: [open-im, open-dialog].");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionWhenActionNotFound() throws InvalidInputException, IOException, ProcessingException {
    String inputMessageML =
        "<messageML><ui-action>" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"action\" is required");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionWithTrigger() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<div class=\"ui-action\" data-action=\"open-im\" data-trigger=\"click\" data-user-ids=\"[123]\">" +
            "<button>Open by stream ID</button>" +
            "</div></div>";

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    validateMessageMLSimpleStructure();
    assertPresentationML(expectedPresentationML);
  }

  @Test
  public void testUIActionWithDefaultTrigger() throws InvalidInputException, IOException, ProcessingException {
    String inputMessageML =
        "<messageML><ui-action action=\"open-im\" user-ids=\"[123]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";
    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<div class=\"ui-action\" data-action=\"open-im\" data-trigger=\"click\" data-user-ids=\"[123]\">" +
            "<button>Open by stream ID</button>" +
            "</div></div>";

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    validateMessageMLSimpleStructure();
    assertPresentationML(expectedPresentationML);
  }

  @Test
  public void testUIActionWithInvalidTrigger() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"double click\" action=\"open-im\" user-ids=\"[123]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"trigger\" of element \"ui-action\" can only be one of the " +
        "following values: [click].");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionUsingUserIdsList() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<div class=\"ui-action\" data-action=\"open-im\" data-trigger=\"click\" data-user-ids=\"[123,456]\">" +
            "<button>Open by stream ID</button>" +
            "</div></div>";

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    validateMessageMLSimpleStructure();
    assertPresentationML(expectedPresentationML);
  }

  @Test
  public void testUIActionUsingMoreThanAllowedUserIds() throws Exception {
    String inputMessageML =
        "<messageML><ui-action action=\"open-im\" user-ids=\"[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"user-ids\" contains more values than allowed. Max value is 15");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionInvalidUserIds() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,abc]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"user-ids\" contains an unsupported format, " +
        "should be an array of user ids");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionUsingStreamId() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\" stream-id=\"UUzhhBMMC+PTHBj0SuoQrn///onSQszYdA==\">"
            +
            "<button title=\"this is a hint\">Open by stream ID</button>" +
            "</ui-action></messageML>";

    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<div class=\"ui-action\" data-action=\"open-im\" data-trigger=\"click\" " +
            "data-stream-id=\"UUzhhBMMC+PTHBj0SuoQrn///onSQszYdA==\">" +
            "<button data-title=\"this is a hint\">Open by stream ID</button>" +
            "</div></div>";

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    validateMessageMLSimpleStructure();
    assertPresentationML(expectedPresentationML);
  }

  @Test
  public void testUIActionUsingUserIdsAndStreamId() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\" " +
            "stream-id=\"UUzhhBMMC+PTHBj0SuoQrn///onSQszYdA==\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Only one between \"stream-id\" and \"user-ids\" is allowed");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionUsingNoUserIdsOrStreamId() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("At least one between \"stream-id\" and \"user-ids\" should be present");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionUsingSideBySide() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\" side-by-side=\"true\">" +
            "<button class=\"primary\" title=\"This is a hint\">Open by stream ID</button>" +
            "</ui-action></messageML>";

    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<div class=\"ui-action\" data-action=\"open-im\" data-trigger=\"click\" data-user-ids=\"[123,456]\" " +
            "data-side-by-side=\"true\">" +
            "<button class=\"primary\" data-title=\"This is a hint\">Open by stream ID</button>" +
            "</div></div>";

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    validateMessageMLSimpleStructure();
    assertPresentationML(expectedPresentationML);
  }

  @Test
  public void testUIActionUsingInvalidSideBySide() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\" side-by-side=\"hello\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"side-by-side\" of element \"ui-action\" can only be one " +
        "of the following values: [true, false].");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionWithoutButtonChild() throws Exception {
    String inputMessageML =
        "<messageML>" +
            "<ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\" side-by-side=\"true\"></ui-action>"
            +
            "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"ui-action\" element must have at least one child that is any of " +
        "the following elements: [button, uiaction].");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionDivChild() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\" side-by-side=\"true\">" +
            "<div>This is a div</div>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"div\" is not allowed in \"ui-action\"");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testNestedUIAction() throws Exception {
    String inputMessageML =
        "<messageML>" +
            "<ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\">" +
            "<ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\">" +
            "<button class=\"secondary\">Open by stream ID</button>" +
            "</ui-action>" +
            "</ui-action>" +
            "</messageML>";

    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<div class=\"ui-action\" data-action=\"open-im\" data-trigger=\"click\" data-user-ids=\"[123,456]\">" +
            "<div class=\"ui-action\" data-action=\"open-im\" data-trigger=\"click\" data-user-ids=\"[123,456]\">" +
            "<button class=\"secondary\">Open by stream ID</button>" +
            "</div>" +
            "</div>" +
            "</div>";

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element uiAction1 = messageML.getChildren().get(0);
    Element uiAction2 = uiAction1.getChildren().get(0);
    Element simpleButton = uiAction2.getChildren().get(0);

    assertEquals(UIAction.class, uiAction1.getClass());
    assertEquals(UIAction.class, uiAction2.getClass());
    assertEquals(Button.class, simpleButton.getClass());
    assertPresentationML(expectedPresentationML);
  }

  @Test
  public void testNestedUIActionWithoutNestedButton() throws Exception {
    String inputMessageML =
        "<messageML>" +
            "<ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\">" +
            "<ui-action trigger=\"click\" action=\"open-im\" user-ids=\"[123,456]\">" +
            "</ui-action>" +
            "</ui-action>" +
            "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"ui-action\" element must have at least one child that is any of " +
        "the following elements: [button, uiaction].");
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionOpenDialogWithoutTargetId() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-dialog\">" +
            "<button>Open the dialog</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"target-id\" is required and must not contain any whitespace");

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionOpenDialogWithoutDialog() throws Exception {
    String inputMessageML =
        "<messageML><ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"dialog-id\">" +
            "<button>Open the dialog</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("ui-action with a target-id must have only one dialog sibling with a matching id");

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionOpenDialogWithoutMatchingDialogId() throws Exception {
    String inputMessageML =
        "<messageML>" +
            "<dialog id=\"target-dialog-id\">" +
            "<title>title</title>" +
            "<body>body</body>" +
            "</dialog>" +
            "<ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"non-matching-id\">" +
            "<button>Open the dialog</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("ui-action with a target-id must have only one dialog sibling with a matching id");

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTargetIdWhenUIActionComesFirst() throws InvalidInputException, IOException, ProcessingException {
    String input = "<messageML>"
        + "<ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"dialogId\">"
        + "<button>Open the Dialog</button>"
        + "</ui-action>"
        + "<dialog id=\"dialogId\">"
        + "<form id=\"all-elements\">"
        + "<title>My Form in a Dialog</title>"
        + "<body>"
        + "<text-field name=\"name\" placeholder=\"Input your name...\"/>"
        + "</body>"
        + "<footer>"
        + "<button type=\"action\" name=\"send-form\">Submit</button>"
        + "<button type=\"reset\">Reset Data</button>"
        + "<button type=\"cancel\" name=\"cancel-form\">Close</button>"
        + "</footer>"
        + "</form>"
        + "</dialog>"
        + "</messageML>";
    String expectedRegex = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<div class=\"ui-action\" data-action=\"open-dialog\" data-trigger=\"click\" data-target-id=\"(\\S+)-dialogId\">"
        + "<button>Open the Dialog</button>"
        + "</div>"
        + "<dialog data-width=\"medium\" data-state=\"close\" id=\"(\\S+)-dialogId\" open=\"\">"
        + "<form id=\"all-elements\">"
        + "<div class=\"dialog-title\">My Form in a Dialog</div>"
        + "<div class=\"dialog-body\"><input type=\"text\" name=\"name\" placeholder=\"Input your name...\"/></div>"
        + "<div class=\"dialog-footer\">"
        + "<button type=\"action\" name=\"send-form\">Submit</button>"
        + "<button type=\"reset\">Reset Data</button>"
        + "<button type=\"cancel\" name=\"cancel-form\">Close</button>"
        + "</div>"
        + "</form>"
        + "</dialog>"
        + "</div>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Pattern expectedPattern = Pattern.compile(expectedRegex);
    Matcher m = expectedPattern.matcher(context.getPresentationML());

    assertTrue(m.find());
    assertEquals(m.group(1), m.group(2));
  }

  @Test
  public void testTargetIdMutipleDialogId() throws InvalidInputException, IOException, ProcessingException {
    String input = "<messageML>"
        + "<ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"dialogId\">"
        + "<button>Open the Dialog</button>"
        + "</ui-action>"
        + "<dialog id=\"dialogId\">"
        + "<form id=\"all-elements\">"
        + "<title>My Form in a Dialog</title>"
        + "<body>"
        + "<text-field name=\"name\" placeholder=\"Input your name...\"/>"
        + "</body>"
        + "<footer>"
        + "<button type=\"action\" name=\"send-form\">Submit</button>"
        + "</footer>"
        + "</form>"
        + "</dialog>"
        + "<dialog id=\"dialogId\">"
        + "<form id=\"all-elements1\">"
        + "<title>My Form in a Dialog</title>"
        + "<body>"
        + "<text-field name=\"name\" placeholder=\"Input your name...\"/>"
        + "</body>"
        + "<footer>"
        + "<button type=\"action\" name=\"send-form\">Submit</button>"
        + "</footer>"
        + "</form>"
        + "</dialog>"
        + "</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Elements must have unique ids. The following value is not unique: [dialogId].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionOpenDialogInDifferentScopeFromDialog() throws Exception {
    String inputMessageML =
        "<messageML>"
            + "<dialog id=\"target-dialog-id\">"
            + "<title>title</title>"
            + "<body>body</body>"
            + "</dialog>"
            + "<form id=\"form-id\">"
            + "<ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"target-dialog-id\">"
            + "<button>Open the dialog</button>"
            + "</ui-action>"
            + "</form>"
            + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The form with id 'form-id' should have at least one action button");

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testUIActionOpenDialogInSameScopeWithDialogWithForm() throws Exception {
    String inputMessageML =
        "<messageML>"
            + "<dialog id=\"target-dialog-id\">"
            + "<form id=\"form-id\">"
            + "<title>title</title>"
            + "<body>body</body>"
            + "</form>"
            + "</dialog>"
            + "<ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"target-dialog-id\">"
            + "<button>Open the dialog</button>"
            + "</ui-action>"
            + "</messageML>";

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    // Check messageML parsing
    final List<Element> children = context.getMessageML().getChildren();

    assertEquals(2, children.size());
    assertEquals(Dialog.class, children.get(0).getClass());

    final Element uiAction = children.get(1);
    assertEquals(UIAction.class, uiAction.getClass());
    assertEquals(Button.class, uiAction.getChildren().get(0).getClass());

    // Check conversion to presentationML
    final String presentationML = context.getPresentationML();

    final String expectedRegex =
        "^<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<dialog data-width=\"medium\" data-state=\"close\" id=\"(\\S+)-target-dialog-id\" open=\"\">"
            + "<form id=\"form-id\">"
            + "<div class=\"dialog-title\">title</div>"
            + "<div class=\"dialog-body\">body</div></form></dialog>"
            + "<div class=\"ui-action\" data-action=\"open-dialog\" data-trigger=\"click\" data-target-id=\"(\\S+)-target-dialog-id\">"
            + "<button>Open the dialog</button></div></div>$";

    Pattern expectedPattern = Pattern.compile(expectedRegex);
    Matcher m = expectedPattern.matcher(presentationML);

    assertTrue(m.find()); // assert presentationML matches the expected pattern
    assertEquals(m.group(1), m.group(2)); // assert the generated IDs are the same
  }

  @Test
  public void testUIActionOpenDialogWithMatchingDialogId() throws Exception {
    String dialogId = "dialog-id";
    String inputMessageML =
        "<messageML>" +
            "<dialog id=\"" + dialogId + "\">" +
            "<title>title</title>" +
            "<body>body</body>" +
            "</dialog>" +
            "<ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"" + dialogId + "\">" +
            "<button>Open the dialog</button>" +
            "</ui-action></messageML>";

    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    // Check messageML parsing
    Element messageML = context.getMessageML();
    final List<Element> children = messageML.getChildren();

    assertEquals(2, children.size());
    assertEquals(Dialog.class, children.get(0).getClass());

    final Element uiAction = children.get(1);
    assertEquals(UIAction.class, uiAction.getClass());
    assertEquals(Button.class, uiAction.getChildren().get(0).getClass());

    // Check conversion to presentationML
    final String presentationML = context.getPresentationML();

    final String expectedRegex =
        "^<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<dialog data-width=\"medium\" data-state=\"close\" id=\"(\\S+)-dialog-id\" open=\"\">"
            + "<div class=\"dialog-title\">title</div>"
            + "<div class=\"dialog-body\">body</div></dialog>"
            + "<div class=\"ui-action\" data-action=\"open-dialog\" data-trigger=\"click\" data-target-id=\"(\\S+)-dialog-id\">"
            + "<button>Open the dialog</button></div>"
            + "</div>$";
    Pattern expectedPattern = Pattern.compile(expectedRegex);
    Matcher m = expectedPattern.matcher(presentationML);

    assertTrue(m.find()); // assert presentationML matches the expected pattern
    assertEquals(m.group(1), m.group(2)); // assert the generated IDs are the same
  }

  @Test
  public void testBiContextOpenIm() throws Exception {
    String inputMessageML =
        "<messageML><ui-action action=\"open-im\" user-ids=\"[123]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    List<BiItem> expectedBiItems =
        Arrays.asList(new BiItem(BiFields.BUTTON.getValue(), new HashMap<>()),
            new BiItem(BiFields.OPENIM.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 1)),
            new BiItem(BiFields.MESSAGE_LENGTH.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 114)));

    List<BiItem> biItems = context.getBiContext().getItems();
    assertIterableEquals(expectedBiItems, biItems);
  }

  @Test
  public void testBiContextOpenDialog() throws Exception {
    String inputMessageML =
        "<messageML>" +
            "<dialog id=\"target-dialog-id\">" +
            "<title>title</title>" +
            "<body>body</body>" +
            "</dialog>" +
            "<ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"target-dialog-id\">" +
            "<button>Open the dialog</button>" +
            "</ui-action></messageML>";
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    List<BiItem> expectedBiItems =
        Arrays.asList(new BiItem(BiFields.POPUPS.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 1)),
            new BiItem(BiFields.BUTTON.getValue(), new HashMap<>()),
            new BiItem(BiFields.OPENDIALOG.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 1)),
            new BiItem(BiFields.MESSAGE_LENGTH.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 220)));

    List<BiItem> biItems = context.getBiContext().getItems();
    assertIterableEquals(expectedBiItems, biItems);
  }

  @Test
  public void testBiContextWithMultipleUIActions() throws Exception {
    String inputMessageML =
        "<messageML>" +
            "<dialog id=\"target-dialog-id\">" +
            "<title>title</title>" +
            "<body>body</body>" +
            "</dialog>" +
            "<ui-action trigger=\"click\" action=\"open-dialog\" target-id=\"target-dialog-id\">" +
            "<button>Open the dialog</button>" +
            "</ui-action>" +
            "<ui-action action=\"open-im\" user-ids=\"[123]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action>" +
            "</messageML>";
    context.parseMessageML(inputMessageML, null, MessageML.MESSAGEML_VERSION);

    List<BiItem> expectedBiItems =
        Arrays.asList(new BiItem(BiFields.POPUPS.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 1)),
            new BiItem(BiFields.BUTTON.getValue(), new HashMap<>()),
            new BiItem(BiFields.OPENDIALOG.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 1)),
            new BiItem(BiFields.BUTTON.getValue(), new HashMap<>()),
            new BiItem(BiFields.OPENIM.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 1)),
            new BiItem(BiFields.MESSAGE_LENGTH.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 311)));

    List<BiItem> biItems = context.getBiContext().getItems();
    assertIterableEquals(expectedBiItems, biItems);
  }

  private void validateMessageMLSimpleStructure() {
    Element messageML = context.getMessageML();
    Element uiAction = messageML.getChildren().get(0);
    Element simpleButton = uiAction.getChildren().get(0);

    assertEquals(UIAction.class, uiAction.getClass());
    assertEquals(Button.class, simpleButton.getClass());
  }

  private void assertPresentationML(String expectedPresentationML) {
    String presentationML = context.getPresentationML();
    assertEquals(expectedPresentationML, presentationML);
  }
}

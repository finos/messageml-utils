package org.finos.symphony.messageml.messagemlutils.elements;

import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.junit.Test;

import static org.junit.Assert.*;

public class SymAiContextTest extends ElementTest {

    @Test
    public void testSymAiContextWithInvalidChild() {
        String invalidChild = "<messageML beta=\"true\"><sym-ai-context><p>invalid child</p></sym-ai-context></messageML>";

        Exception exception = assertThrows(InvalidInputException.class, () -> context.parseMessageML(invalidChild, null, null));
        String expectedMessage = "Element 'p' is not allowed in 'sym-ai-context'. Allowed elements are: [stream, message, attachment]";
        String actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(expectedMessage));
    }

    @Test
    public void testSymAiContextWithText() {
        String textChild = "<messageML beta=\"true\"><sym-ai-context>text</sym-ai-context></messageML>";

        Exception exception = assertThrows(InvalidInputException.class, () -> context.parseMessageML(textChild, null, null));
        String expectedMessage = "Element \"sym-ai-context\" may not have text content";
        String actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(expectedMessage));
    }

    @Test
    public void testSymAiContextWithValidChildren() throws Exception {
        String validChildren = "<messageML beta=\"true\"><sym-ai-context>"
                + "<stream id=\"stream1\"/>"
                + "<message id=\"msg1\"/>"
                + "<attachment streamId=\"stream2\" messageId=\"msg2\" fileId=\"file1\"/>"
                + "</sym-ai-context></messageML>";
        context.parseMessageML(validChildren, null, null);

        Element element = context.getMessageML().getChildren().get(0);
        assertEquals(SymAiContext.class, element.getClass());

        String expectedEntityJson = "{\"sym-ai-context1\":{\"type\":\"com.symphony.ai.context\","
                + "\"version\":\"1.0\","
                + "\"messages\":[{\"id\":\"msg1\"}],"
                + "\"streams\":[{\"id\":\"stream1\"}],"
                + "\"attachments\":[{\"id\":\"file1\",\"streamId\":\"stream2\",\"messageId\":\"msg2\"}]}}";
        assertEquals(expectedEntityJson, context.getEntityJson().toString());

        String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\"><span class=\"entity\" data-entity-id=\"sym-ai-context1\"></span></div>";
        assertEquals(expectedPresentationML, context.getPresentationML());
    }

    @Test
    public void testSymAiContextNotAllowedWithoutBeta() {
        String input = "<messageML><sym-ai-context>"
                + "<stream id=\"stream1\"/>"
                + "</sym-ai-context></messageML>";

        Exception exception = assertThrows(InvalidInputException.class, () -> context.parseMessageML(input, null, null));
        String expectedMessage = "Element \"sym-ai-context\" is only allowed when messageML has beta=\"true\"";
        assertEquals(expectedMessage, exception.getMessage());
    }

    @Test
    public void testSymAiContextNotAllowedWithBetaFalse() {
        String input = "<messageML beta=\"false\"><sym-ai-context>"
                + "<stream id=\"stream1\"/>"
                + "</sym-ai-context></messageML>";

        Exception exception = assertThrows(InvalidInputException.class, () -> context.parseMessageML(input, null, null));
        String expectedMessage = "Element \"sym-ai-context\" is only allowed when messageML has beta=\"true\"";
        assertEquals(expectedMessage, exception.getMessage());
    }

}

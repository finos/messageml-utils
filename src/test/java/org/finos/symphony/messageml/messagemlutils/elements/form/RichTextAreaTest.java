package org.finos.symphony.messageml.messagemlutils.elements.form;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.elements.ElementTest;
import org.finos.symphony.messageml.messagemlutils.elements.MessageML;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;


public class RichTextAreaTest extends ElementTest {



  @ParameterizedTest
  @MethodSource("messageMlStream")
  public void parseAndGenerate(String messageMl, String presentationML, String markdown, Map<String, Object> expectedAttributes) throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);

    String input = String.format(
        "<messageML>\n "
            + "<form id=\"form_id\">\n "
            + "%s\n"
            + "<button name=\"name01\">Submit</button>\n "
            + "</form>\n </messageML>",
        messageMl);

    messageMLContext.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String actualPML = messageMLContext.getPresentationML();

    assertEquals(presentationML, actualPML);

    assertEquals(markdown, messageMLContext.getMarkdown());



    List<BiItem> items = messageMLContext.getBiContext().getItems();


    BiItem textAreaBiItemExpected = new BiItem(BiFields.RICH_TEXT_AREA.getValue(), expectedAttributes);

    assertEquals(4, items.size());
    assertEquals(BiFields.RICH_TEXT_AREA.getValue(), items.get(0).getName());
    assertSameBiItem(textAreaBiItemExpected, items.get(0));
    assertMessageLengthBiItem(items.get(3), input.length());
  }

  private static Stream<Arguments> messageMlStream() {
    return Stream.of(
        Arguments.of(
            "<richtextarea name=\"req\" required=\"true\" "
                + "pattern=\"^[a-zA-Z]{3,3}$\" pattern-error-message=\"error message\" "
                + "placeholder=\"placeholder01\" formnovalidate=\"true\"> With initial value</richtextarea>\n",
            "<div data-format=\"PresentationML\" data-version=\"2.0\">  <form id=\"form_id\">  "
              + "<textarea data-formnovalidate=\"true\" data-richtext=\"true\" name=\"req\" placeholder=\"placeholder01\" required=\"true\" pattern=\"^[a-zA-Z]{3,3}$\" data-pattern-error-message=\"error message\">"
                + " With initial value"
                + "</textarea> "
                + "<button type=\"action\" name=\"name01\">Submit</button>  "
                + "</form>  </div>",
            "  \n   \n  (Rich Text Area:[placeholder01] With initial value) (Button:Submit)  \n   \n  ",
            Stream.of(new Object[][] {
                {BiFields.PLACEHOLDER.getValue(), 1},
                {BiFields.DEFAULT.getValue(), 1},
                {BiFields.REQUIRED.getValue(), 1},
                {BiFields.VALIDATION_PATTERN.getValue(), 1},
                {BiFields.VALIDATION.getValue(), 1},
            }).collect(Collectors.toMap(property -> property[0], property -> property[1]))),



        Arguments.of(
            "<richtextarea name=\"req\"/>\n",
            "<div data-format=\"PresentationML\" data-version=\"2.0\">  "
                + "<form id=\"form_id\">  "
                + "<textarea name=\"req\" data-richtext=\"true\"></textarea> "
                + "<button type=\"action\" name=\"name01\">Submit</button>  "
                + "</form>  "
                + "</div>",
            "  \n   \n  (Rich Text Area) (Button:Submit)  \n   \n  ",
            Collections.emptyMap())
    );
  }
}

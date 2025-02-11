package org.finos.symphony.messageml.messagemlutils.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import com.fasterxml.jackson.databind.JsonNode;
import org.apache.commons.io.IOUtils;
import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;
import org.finos.symphony.messageml.messagemlutils.util.TestDataProvider;
import org.finos.symphony.messageml.messagemlutils.util.instrument.resolver.ResolutionResults;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class TagTest extends ElementTest {

  @Test
  public void testTagWithAllValidAttributes() throws Exception {
    ResolutionResults results = retrieveJsonPayload("finref_response");
    TestDataProvider.class.cast(dataProvider).setResolutionResults(results);
    String input =
        "<messageML><tag fullbbgcompticker=\"fullbbgcompticker\" unique-id=\"unique-id\" "
            + "figi=\"figi\" "
            + "bbgcompticker=\"bbgcompticker\" figi-ticker=\"figi-ticker\" "
            + "us-code=\"us-code\" isin=\"isin\" local-code=\"local-code\" "
            + "instrument-class=\"equity\" "
            + "bbgmarket-sector=\"Equity\" "
            + "return-main-listing=\"return-main-listing\" "
            + "country-code=\"country-code\" operational-mic=\"operational-mic\" "
            + "fallback-ticker=\"fallback-ticker\"/></messageML>";
    String expectedMarkdown = "$000930";
    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><span class=\"entity\" "
            + "data-entity-id=\"tag1\">$000930</span></div>";
    String expectedJson =
        "{\"tag1\":{\"type\":\"org.symphonyoss.fin.security\",\"version\":\"2.0\","
            + "\"id\":[{\"type\":\"org.symphonyoss.fin.security.id.ticker\","
            + "\"value\":\"000930\"},{\"type\":\"org.symphonyoss.fin.security.id.uniqueId\","
            + "\"value\":\"831bb1ae-7ccc-4d48-a5f3-868e197db1ba\"},{\"type\":\"org.symphonyoss"
            + ".fin.security.id.fullBbgTicker\",\"value\":\"000930 CH Equity\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.bbgcompticker\",\"value\":\"000930 CH\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.id.isin\",\"value\":\"CNE000000ZR7\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.id.figi\",\"value\":\"BBG000DYGW93\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.id.figiTicker\",\"value\":\"000930 CS\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.id.lei\",\"value\":\"000930 CS\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.id.localCode\",\"value\":\"000930\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.id.operationalMic\",\"value\":\"XSHE\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.countryCode\",\"value\":\"CN\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.countryName\",\"value\":\"China\"},{\"type\":\"org"
            + ".symphonyoss.fin.security.exchangeName\",\"value\":\"Shenzhen Stock Exchange\"},"
            + "{\"type\":\"org.symphonyoss.fin.security.displayName\",\"value\":\"Cofco "
            + "Biotechnology Co Ltd\"},{\"type\":\"org.symphonyoss.fin.security.currency\","
            + "\"value\":\"CNY\"},{\"type\":\"org.symphonyoss.fin.security.instrumentTypeCode\","
            + "\"value\":\"EQS\"},{\"type\":\"org.symphonyoss.fin.security.instrumentTypeName\","
            + "\"value\":\"Equity Shares\"}]}}";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    JsonNode node = context.getEntities();
    verifyTag(messageML, results, expectedPresentationML, expectedJson, expectedMarkdown);
  }

  @Test
  public void testTagWithInstrumentNotFoundAndFallbackTicker() throws Exception {
    ResolutionResults results = retrieveJsonPayload("finref_with_instrument_not_found_response");
    TestDataProvider.class.cast(dataProvider).setResolutionResults(results);
    String input =
        "<messageML><tag fullbbgcompticker=\"226 HK Equity\" "
            + "fallback-ticker=\"fallback\"/></messageML>";
    String expectedMarkdown = "$fallback";
    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><span class=\"entity\" "
            + "data-entity-id=\"tag1\">$fallback</span></div>";
    String expectedJson =
        "{\"tag1\":{\"type\":\"org.symphonyoss.fin.security\",\"version\":\"1.0\","
            + "\"id\":[{\"type\":\"org.symphonyoss.fin.security.id.ticker\","
            + "\"value\":\"fallback\"}]}}";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    JsonNode node = context.getEntities();
    verifyTag(messageML, results, expectedPresentationML, expectedJson, expectedMarkdown);
  }


  @Test
  public void testTagWithInstrumentNotFoundAndNoFallbackTicker() throws Exception {
    ResolutionResults results = retrieveJsonPayload("finref_with_instrument_not_found_response");
    TestDataProvider.class.cast(dataProvider).setResolutionResults(results);
    String input = "<messageML><tag fullbbgcompticker=\"226 HK Equity\"/></messageML>";
    InvalidInputException exception = assertThrows(InvalidInputException.class, () ->
        context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION)
    );
    assertEquals("Exception Message",
        "No instrument found , \"fallback-ticker\" attribute is required", exception.getMessage());
  }

  @Test
  public void testBiContextTagEntity() throws InvalidInputException, IOException,
      ProcessingException {
    ResolutionResults results = retrieveJsonPayload("finref_response");
    TestDataProvider.class.cast(dataProvider).setResolutionResults(results);
    String input = "<messageML><tag fullbbgcompticker=\"226 HK Equity\"/></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = context.getBiContext().getItems();

    Map<String, Object> tagExpectedAttributes =
        Collections.singletonMap(BiFields.COUNT.getValue(), 1);
    Map<String, Object> entityExpectedAttributes =
        Collections.singletonMap(BiFields.ENTITY_TYPE.getValue(), "org.symphonyoss.fin.security");

    BiItem mentionBiItemExpected =
        new BiItem(BiFields.HASHTAGS.getValue(), tagExpectedAttributes);
    BiItem entityBiItemExpected = new BiItem(BiFields.ENTITY.getValue(), entityExpectedAttributes);

    assertEquals(3, items.size());
    assertSameBiItem(mentionBiItemExpected, items.get(0));
    assertSameBiItem(entityBiItemExpected, items.get(1));
    assertMessageLengthBiItem(items.get(2), input.length());
  }

  private ResolutionResults retrieveJsonPayload(String fileName) throws IOException {
    ClassLoader classLoader = getClass().getClassLoader();
    InputStream stream = classLoader.getResourceAsStream("payloads/" + fileName + ".json");
    String response = IOUtils.toString(stream, StandardCharsets.UTF_8);
    return MAPPER.readValue(response, ResolutionResults.class);
  }

  private void verifyTag(Element messageML, ResolutionResults user, String expectedPresentationML,
      String expectedJson, String expectedMarkdown)
      throws Exception {
    assertEquals("Element children", 1, messageML.getChildren().size());
    Element tag = messageML.getChildren().get(0);
    assertEquals("Element class", Tag.class, tag.getClass());
    assertEquals("Element tag name", "tag", tag.getMessageMLTag());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("EntityJSON", expectedJson, MAPPER.writeValueAsString(context.getEntityJson()));
    assertEquals("Legacy entities", 1, context.getEntities().size());
  }
}

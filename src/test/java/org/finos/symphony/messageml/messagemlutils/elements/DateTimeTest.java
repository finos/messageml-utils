package org.finos.symphony.messageml.messagemlutils.elements;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DateTimeTest extends ElementTest {
  @Test
  public void timeDate_works() throws InvalidInputException, IOException, ProcessingException {
    String input = "<messageML>\n"
        + "<dateTime value=\"2024-12-31T09:29:47Z\" format=\"date\"/>\n"
        + "<dateTime value=\"2025-12-31T09:29:47Z\" format=\"time\"/>\n"
        + "<dateTime value=\"2026-12-31T09:29:47Z\" format=\"time_secs\"/>\n"
        + "<dateTime value=\"2027-12-31T09:29:47Z\"/>\n"
        + "</messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\"> "
        + "<span><time data-entity-id=\"datetime1\" datetime=\"2024-12-31T09:29:47Z\" data-format=\"date\">2024-12-31T09:29:47Z</time></span> "
        + "<span><time data-entity-id=\"datetime2\" datetime=\"2025-12-31T09:29:47Z\" data-format=\"time\">2025-12-31T09:29:47Z</time></span> "
        + "<span><time data-entity-id=\"datetime3\" datetime=\"2026-12-31T09:29:47Z\" data-format=\"time_secs\">2026-12-31T09:29:47Z</time></span> "
        + "<span><time data-entity-id=\"datetime4\" datetime=\"2027-12-31T09:29:47Z\">2027-12-31T09:29:47Z</time></span> "
        + "</div>";
    String expectedMarkdown = " 2024-12-31T09:29:47Z 2025-12-31T09:29:47Z 2026-12-31T09:29:47Z 2027-12-31T09:29:47Z ";
    assertEquals(expectedPresentationML, context.getPresentationML());
    assertEquals(expectedMarkdown, context.getMarkdown());

    JsonNode entities = context.getEntities();

    assertNotNull(entities.get("datetimes"));

    JsonNode datetimes = entities.get("datetimes");

    assertTrue(datetimes.isArray());
    verifyEntity(datetimes.get(0), "datetime1", "2024-12-31T09:29:47Z", "date", 1, 21);
    verifyEntity(datetimes.get(1), "datetime2", "2025-12-31T09:29:47Z", "time", 22, 42);
    verifyEntity(datetimes.get(2), "datetime3", "2026-12-31T09:29:47Z", "time_secs", 43, 63);
    verifyEntity(datetimes.get(3), "datetime4", "2027-12-31T09:29:47Z",  null, 64, 84);


    ObjectNode entityJson = context.getEntityJson();

    verifyEntityJson(entityJson, "datetime1", "2024-12-31T09:29:47Z", "date");
    verifyEntityJson(entityJson, "datetime2", "2025-12-31T09:29:47Z", "time");
    verifyEntityJson(entityJson, "datetime3", "2026-12-31T09:29:47Z", "time_secs");
    verifyEntityJson(entityJson, "datetime4", "2027-12-31T09:29:47Z",  null);

    BiContext biContext = context.getBiContext();


    Map<String, Object> dateTimeExpectedAttributes =
        Collections.singletonMap(BiFields.COUNT.getValue(), 4);
    Map<String, Object> entityExpectedAttributes =
        Collections.singletonMap(BiFields.ENTITY_TYPE.getValue(), "org.symphonyoss.datetime");

    BiItem dateTimeBiItemExpected = new BiItem(BiFields.DATE_TIME.getValue(), dateTimeExpectedAttributes);
    BiItem entityBiItemExpected = new BiItem(BiFields.ENTITY.getValue(), entityExpectedAttributes);

    assertEquals(6, biContext.getItems().size());
    assertSameBiItem(dateTimeBiItemExpected, biContext.getItems().get(0));
    assertSameBiItem(entityBiItemExpected, biContext.getItems().get(1));
    assertSameBiItem(entityBiItemExpected, biContext.getItems().get(2));
    assertSameBiItem(entityBiItemExpected, biContext.getItems().get(3));
    assertSameBiItem(entityBiItemExpected, biContext.getItems().get(4));
    assertMessageLengthBiItem(biContext.getItems().get(5), input.length());
  }

  @Test
  public void dateTime_valueIsRequired() {
    String input = "<messageML><dateTime format=\"date\" /></messageML>";

    InvalidInputException exception = assertThrows(InvalidInputException.class, () -> context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION));

    assertEquals("The attribute \"value\" is required", exception.getMessage());
  }

  @Test
  public void dateTime_valueMustBeValid() {
    String input = "<messageML><dateTime value=\"2024-12-01\"  /></messageML>";

    InvalidInputException exception = assertThrows(InvalidInputException.class, () -> context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION));

    assertEquals("Attribute \"value\" has invalid date format", exception.getMessage());
  }

  @Test
  public void dateTime_formatMustBeValid() {
    String input = "<messageML><dateTime value=\"2027-12-31T09:29:47Z\" format=\"test\" /></messageML>";

    InvalidInputException exception = assertThrows(InvalidInputException.class, () -> context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION));

    assertEquals("Attribute \"format\" of element \"dateTime\" can only be one of the following values: [date_time, date, time, time_secs].", exception.getMessage());
  }

  private void verifyEntity(JsonNode entity, String id, String value, String format, int start, int end) {
    assertEquals(id, entity.get("id").asText());
    assertEquals(value, entity.get("text").asText());
    assertEquals(value, entity.get("value").asText());
    if (format != null) {
      assertEquals(format, entity.get("format").asText());
    } else {
      assertNull(entity.get("format"), "format must be null");
    }
    assertEquals(start, entity.get("indexStart").asInt());
    assertEquals(end, entity.get("indexEnd").asInt());
    assertEquals("DATE_TIME", entity.get("type").asText());
  }
  
  private void verifyEntityJson(ObjectNode entityJson, String id, String value, String format) {
    assertNotNull(entityJson.get(id));
    JsonNode entity = entityJson.get(id);
    
    assertEquals("org.symphonyoss", entity.get("type").asText());
    assertEquals("1.0", entity.get("version").asText());
    
    assertNotNull(entity.get("data"));
    JsonNode data = entity.get("data");
    assertEquals("org.symphonyoss.datetime", data.get("type").asText());
    assertEquals(value, data.get("value").asText());
    if (format != null) {
      assertEquals(format, data.get("format").asText());
    } else {
      assertTrue(data.get("format").isNull(), "format must be null");
    }
  }
}

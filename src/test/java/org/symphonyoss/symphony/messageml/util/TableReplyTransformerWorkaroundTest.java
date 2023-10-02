package org.symphonyoss.symphony.messageml.util;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Assert;
import org.junit.Test;

public class TableReplyTransformerWorkaroundTest {
  private static final ObjectMapper MAPPER = new ObjectMapper();


  @Test
  public void testReplaceEmphasisTableReplyWithoutMediaTable() throws Exception {

    StringBuilder markdown = new StringBuilder("_Hello [TABLE] world!_");
    JsonNode media = MAPPER.readTree("{}");
    TableReplyTransformerWorkaround.replaceEmphasis(markdown, media);
    assertEquals("Output", "_Hello [TABLE] world!_", markdown.toString());
  }

  @Test
  public void testReplaceEmphasisTableReplyWithMediaTable() throws Exception {

    StringBuilder markdown = new StringBuilder("_Hello \u0091TABLE\u0092 world!_");
    JsonNode media = MAPPER.readTree(
        "{\"mediaType\":\"JSON\",\"content\":[{\"type\":\"excel-rcp\",\"title\":\"\","
            + "\"text\":[[\"Overall\",\"77:23 buys:sells\",\"% of Volume\"],[\"0-2y\",\"72:28 "
            + "b:s\",\"41%\"],[\"3-5y\",\"80:20 b:s\",\"32%\"],[\"6-9y\",\"83:17 b:s\",\"19%\"],"
            + "[\"10-12y\",\"78:22 b:s\",\"5%\"],[\"13y+\",\"69:31 b:s\",\"4%\"]],\"index\":0}]}");
    TableReplyTransformerWorkaround.replaceEmphasis(markdown, media);
    assertEquals("Output", " Hello \u0091TABLE\u0092 world! ", markdown.toString());
  }



}



/*
 * Copyright 2016-2017 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.symphonyoss.symphony.messageml;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.MarkdownParser;
import org.symphonyoss.symphony.messageml.markdown.MarkdownRenderer;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * The main entry point for parsing string message data in MessageML or Markdown formats and associated JSON entity
 * data into {@link MessageML} document trees and converting them to output formats (PresentationML and Markdown).
 *
 * To generate a messageML tree, use one of parseMessageML() or parseMarkdown(). Once the message has been parsed,
 * use one of the get() methods to retrieve the desired output format.
 * @author lukasz
 * @since 3/21/17
 */
public class MessageMLContext {

  private final MessageMLParser messageMLParser;
  private final MarkdownParser markdownParser;
  private MarkdownRenderer markdownRenderer;
  private MessageML messageML;
  private ObjectNode entityJson;

  public MessageMLContext(IDataProvider dataProvider) {
    this.markdownParser = new MarkdownParser(dataProvider);
    this.messageMLParser = new MessageMLParser(dataProvider);
  }

  /**
   * Parse the text contents of the message and optionally EntityJSON into a MessageMLV2 message. Expands
   * Freemarker templates and generates document tree structures for serialization into output formats with the
   * respective get() methods.
   * @param message string containing a MessageMLV2 message with optional Freemarker templates
   * @param entityJson string containing EntityJSON data
   * @param version string containing the version of the message format
   * @throws InvalidInputException thrown on invalid MessageMLV2 input
   * @throws ProcessingException thrown on errors generating the document tree
   * @throws IOException thrown on invalid EntityJSON input
   */
  public void parseMessageML(String message, String entityJson, String version) throws InvalidInputException, IOException,
      ProcessingException {

    this.messageML = messageMLParser.parse(message, entityJson, version);
    this.entityJson = messageMLParser.getEntityJson();
    this.markdownRenderer = new MarkdownRenderer(messageML.asMarkdown());
  }

  /**
   * Parse a Markdown message into its MessageMLV2 representation. Generates document tree structures for
   * serialization into output formats with the respective get() methods.
   * @param message string containing a message in Markdown
   * @param entities additional entity data in JSON
   */
  public void parseMarkdown(String message, JsonNode entities, JsonNode media) throws InvalidInputException {
    this.messageML = markdownParser.parse(message, entities, media);
    this.entityJson = messageML.asEntityJson(this.entityJson);
    this.markdownRenderer = new MarkdownRenderer(messageML.asMarkdown());
  }

  /**
   * Retrieve the MessageML document tree.
   * @throws IllegalStateException thrown if the message hasn't been parsed yet
   */
  public MessageML getMessageML() throws IllegalStateException {
    if (messageML == null) {
      throw new IllegalStateException("The message hasn't been parsed yet. "
          + "Please call MessageMLContext.parse() first.");
    }

    return messageML;
  }

  /**
   * Retrieve a string representation of the message in PresentationML.
   * @throws IllegalStateException thrown if the message hasn't been parsed yet
   */
  public String getPresentationML() throws IllegalStateException {
    if (messageML == null) {
      throw new IllegalStateException("The message hasn't been parsed yet. "
          + "Please call MessageMLContext.parse() first.");
    }

    ByteArrayOutputStream bout = new ByteArrayOutputStream();
    XmlPrintStream out = new XmlPrintStream(bout);

    out.setNoIndent(true);
    out.setNoNl(true);

    messageML.asPresentationML(out);

    out.close();

    return bout.toString();
  }

  /**
   * Retrieve a JSON representation of entity data (EntityJSON).
   */
  public ObjectNode getEntityJson() {
    if (messageML == null) {
      throw new IllegalStateException("The message hasn't been parsed yet. "
          + "Please call MessageMLContext.parse() first.");
    }

    return entityJson;
  }

  /**
   * Retrieve a string representation of the message in Markdown.
   * @throws IllegalStateException thrown if the message hasn't been parsed yet
   */
  public String getMarkdown() throws IllegalStateException {
    if (messageML == null) {
      throw new IllegalStateException("The message hasn't been parsed yet. "
          + "Please call MessageMLContext.parse() first.");
    }

    return markdownRenderer.getText();
  }

  /**
   * Retrieve message entities (tags, mentions, urls) in the legacy JSON format.
   * @throws IllegalStateException thrown if the message hasn't been parsed yet
   */
  public JsonNode getEntities() throws IllegalStateException {
    if (messageML == null) {
      throw new IllegalStateException("The message hasn't been parsed yet. "
          + "Please call MessageMLContext.parse() first.");
    }

    return markdownRenderer.getJson();
  }

}

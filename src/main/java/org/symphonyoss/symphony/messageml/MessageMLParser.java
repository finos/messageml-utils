package org.symphonyoss.symphony.messageml;

import static org.symphonyoss.symphony.messageml.elements.Element.CLASS_ATTR;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import org.apache.commons.io.input.ReaderInputStream;
import org.apache.commons.lang3.StringUtils;
import org.symphonyoss.symphony.messageml.elements.Bold;
import org.symphonyoss.symphony.messageml.elements.BulletList;
import org.symphonyoss.symphony.messageml.elements.Card;
import org.symphonyoss.symphony.messageml.elements.CardBody;
import org.symphonyoss.symphony.messageml.elements.CardHeader;
import org.symphonyoss.symphony.messageml.elements.CashTag;
import org.symphonyoss.symphony.messageml.elements.Chime;
import org.symphonyoss.symphony.messageml.elements.Div;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.Entity;
import org.symphonyoss.symphony.messageml.elements.FormatEnum;
import org.symphonyoss.symphony.messageml.elements.HashTag;
import org.symphonyoss.symphony.messageml.elements.Header;
import org.symphonyoss.symphony.messageml.elements.HorizontalRule;
import org.symphonyoss.symphony.messageml.elements.Image;
import org.symphonyoss.symphony.messageml.elements.Italic;
import org.symphonyoss.symphony.messageml.elements.LineBreak;
import org.symphonyoss.symphony.messageml.elements.Link;
import org.symphonyoss.symphony.messageml.elements.ListItem;
import org.symphonyoss.symphony.messageml.elements.Mention;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.OrderedList;
import org.symphonyoss.symphony.messageml.elements.Paragraph;
import org.symphonyoss.symphony.messageml.elements.Span;
import org.symphonyoss.symphony.messageml.elements.Table;
import org.symphonyoss.symphony.messageml.elements.TableBody;
import org.symphonyoss.symphony.messageml.elements.TableCell;
import org.symphonyoss.symphony.messageml.elements.TableFooter;
import org.symphonyoss.symphony.messageml.elements.TableHeader;
import org.symphonyoss.symphony.messageml.elements.TableHeaderCell;
import org.symphonyoss.symphony.messageml.elements.TableRow;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.NoOpEntityResolver;
import org.symphonyoss.symphony.messageml.util.NullErrorHandler;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

/**
 * Converts a string representation of the message and optional entity data into a MessageMLV2 document tree.
 * @author lukasz
 * @since 4/20/17
 */
public class MessageMLParser {
  private static final ObjectMapper MAPPER = new ObjectMapper();
  private static final Configuration FREEMARKER = new Configuration(Configuration.getVersion());
  private final IDataProvider dataProvider;

  private FormatEnum messageFormat;
  private MessageML messageML;
  private ObjectNode entityJson;
  private int size;

  static {
    FREEMARKER.setDefaultEncoding("UTF-8");
    FREEMARKER.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
    FREEMARKER.setLogTemplateExceptions(false);
  }

  MessageMLParser(IDataProvider dataProvider) {
    this.dataProvider = dataProvider;
  }

  /**
   * Parse the text contents of the message and optionally EntityJSON into a MessageMLV2 message. Expands
   * Freemarker templates and generates a MessageML document tree.
   * @param message string containing a MessageMLV2 message with optional Freemarker templates
   * @param entityJson string containing EntityJSON data
   * @param version string containing the version of the message format
   * @throws InvalidInputException thrown on invalid MessageMLV2 input
   * @throws ProcessingException thrown on errors generating the document tree
   * @throws IOException thrown on invalid EntityJSON input
   */
  MessageML parse(String message, String entityJson, String version) throws InvalidInputException, ProcessingException,
      IOException {
    this.size = 0;

    String expandedMessage;

    if (StringUtils.isNotBlank(entityJson)) {
      try {
        this.entityJson = (ObjectNode) MAPPER.readTree(entityJson);
      } catch (JsonProcessingException e) {
        throw new InvalidInputException("Error parsing EntityJSON: " + e.getMessage());
      }
    } else {
      this.entityJson = new ObjectNode(JsonNodeFactory.instance);
    }

    try {
      expandedMessage = expandTemplates(message, this.entityJson);
    } catch (IOException e) {
      throw new InvalidInputException("Error parsing EntityJSON: " + e.getMessage());
    } catch (TemplateException e) {
      throw new InvalidInputException(String.format("Error parsing Freemarker template: invalid input at line %s, "
          + "column %s", e.getLineNumber(), e.getColumnNumber()));
    }

    this.messageML = parseMessageML(expandedMessage, version);

    if (this.messageML != null) {
      ObjectNode generatedEntities = this.messageML.asEntityJson();
      this.entityJson.setAll(generatedEntities);

      return this.messageML;
    }

    throw new ProcessingException("Internal error. Generated null MessageML from valid input");
  }

  /**
   * Retrieve a JSON representation of entity data (EntityJSON).
   */
  ObjectNode getEntityJson() {
    return entityJson;
  }

  /**
   * Check the input message text for null value and restricted characters.
   */
  private static void validateMessageText(String messageML) throws InvalidInputException {
    if (messageML == null) { throw new InvalidInputException("Message input is NULL"); }

    for (char ch : messageML.toCharArray()) {
      if (ch != '\n' && ch != '\r' && ch != '\t' && (ch < ' ')) {
        throw new InvalidInputException("Invalid control characters in message");
      }
    }
  }

  /**
   * Check whether <i>data-entity-id</i> attributes in the message match EntityJSON entities.
   */
  private static void validateEntities(org.w3c.dom.Element document, JsonNode entityJson) throws InvalidInputException,
      ProcessingException {
    XPathFactory xPathfactory = XPathFactory.newInstance();
    XPath xpath = xPathfactory.newXPath();

    NodeList nodes;
    try {
      XPathExpression expr = xpath.compile("//@data-entity-id");
      nodes = (NodeList) expr.evaluate(document, XPathConstants.NODESET);
    } catch (XPathExpressionException e) {
      throw new ProcessingException("Internal error processing document tree: " + e.getMessage());
    }

    for (int i = 0; i < nodes.getLength(); i++) {
      Node node = nodes.item(i);
      String entityId = ((org.w3c.dom.Attr) node).getValue();

      if (entityJson.findPath(entityId).isMissingNode()) {
        throw new InvalidInputException("Error processing EntityJSON: "
            + "no entity data provided for \"data-entity-id\"=\"" + entityId + "\"");
      }
    }
  }

  /**
   * Throw an exception if the enclosing message is in PresentationML and a MessageML tag is used.
   */
  private void validatePresentationML(String tag) throws InvalidInputException {
    if (messageFormat == FormatEnum.PRESENTATIONML) {
      throw new InvalidInputException("Shorthand tag \"" + tag + "\" is not allowed in PresentationML");
    }
  }

  /**
   * Expand Freemarker templates.
   */
  private String expandTemplates(String message, JsonNode entityJson) throws IOException, TemplateException {
    // Read entityJSON data
    Map<String, Object> data = new HashMap<>();
    data.put("entity", MAPPER.convertValue(entityJson, Map.class));

    // Read MessageMLV2 template
    StringWriter sw = new StringWriter();
    Template template = new Template("messageML", message, FREEMARKER);

    // Expand the template
    template.process(data, sw);

    return sw.toString();
  }

  /**
   * Parse the message string into its MessageML representation.
   */
  private MessageML parseMessageML(String messageML, String version) throws InvalidInputException, ProcessingException {
    validateMessageText(messageML);

    org.w3c.dom.Element docElement = parseDocument(messageML);

    validateEntities(docElement, entityJson);

    switch (docElement.getTagName()) {
      case MessageML.MESSAGEML_TAG:
        this.messageFormat = FormatEnum.MESSAGEML;
        if (StringUtils.isBlank(version)) {
          version = MessageML.MESSAGEML_VERSION;
        }
        break;

      case MessageML.PRESENTATIONML_TAG:
        this.messageFormat = FormatEnum.PRESENTATIONML;
        break;

      default:
        throw new InvalidInputException("Root tag must be <" + MessageML.MESSAGEML_TAG + ">"
            + " or <" + MessageML.PRESENTATIONML_TAG + ">");
    }

    MessageML result = new MessageML(messageFormat, version);
    result.buildAll(this, docElement);
    result.validate();

    return result;
  }

  /**
   * Parse the message string into a DOM element tree.
   */
  private org.w3c.dom.Element parseDocument(String messageML)
      throws InvalidInputException, ProcessingException {
    try {
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      //XXE prevention as per https://www.owasp.org/index.php/XML_External_Entity_(XXE)_Prevention_Cheat_Sheet
      dbFactory.setXIncludeAware(false);
      dbFactory.setExpandEntityReferences(false);
      dbFactory.setIgnoringElementContentWhitespace(true);
      dbFactory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
      dbFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);

      DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
      dBuilder.setErrorHandler(new NullErrorHandler()); // default handler prints to stderr
      dBuilder.setEntityResolver(new NoOpEntityResolver());

      StringReader sr = new StringReader(messageML);
      ReaderInputStream ris = new ReaderInputStream(sr);
      Document doc = dBuilder.parse(ris);

      doc.getDocumentElement().normalize();

      return doc.getDocumentElement();

    } catch (SAXException e) {
      throw new InvalidInputException("Invalid messageML: " + e.getMessage(), e);
    } catch (ParserConfigurationException | IOException e) {
      throw new ProcessingException("Failed to parse messageML", e);
    }
  }

  /**
   * Create a MessageML element based on the DOM element's name and attributes.
   */
  public Element createElement(org.w3c.dom.Element element, Element parent) throws
      InvalidInputException {
    String tag = element.getNodeName();

    if (Header.isHeaderElement(tag)) {
      return new Header(++size, parent, tag);
    }

    switch (tag) {
      case Chime.MESSAGEML_TAG:
        validatePresentationML(tag);
        return new Chime(++size, parent, FormatEnum.MESSAGEML);

      case Chime.PRESENTATIONML_TAG:
        return new Chime(++size, parent, FormatEnum.PRESENTATIONML);

      case Paragraph.MESSAGEML_TAG:
        return new Paragraph(++size, parent);

      case LineBreak.MESSAGEML_TAG:
        return new LineBreak(++size, parent);

      case HorizontalRule.MESSAGEML_TAG:
        return new HorizontalRule(++size, parent);

      case Span.MESSAGEML_TAG:
        switch (element.getAttribute(CLASS_ATTR)) {
          case Entity.PRESENTATIONML_CLASS:
            return createEntity(element, parent);
          default:
            return new Span(++size, parent);
        }

      case Div.MESSAGEML_TAG:
        switch (element.getAttribute(CLASS_ATTR)) {
          case Card.PRESENTATIONML_CLASS:
            element.removeAttribute(CLASS_ATTR);
            return new Card(++size, parent, FormatEnum.PRESENTATIONML);
          case CardBody.PRESENTATIONML_CLASS:
            element.removeAttribute(CLASS_ATTR);
            return new CardBody(++size, parent, FormatEnum.PRESENTATIONML);
          case CardHeader.PRESENTATIONML_CLASS:
            element.removeAttribute(CLASS_ATTR);
            return new CardHeader(++size, parent, FormatEnum.PRESENTATIONML);
          default:
            return new Div(++size, parent);
        }

      case Bold.MESSAGEML_TAG:
        return new Bold(++size, parent);

      case Italic.MESSAGEML_TAG:
        return new Italic(++size, parent);

      case HashTag.MESSAGEML_TAG:
        validatePresentationML(tag);
        return new HashTag(++size, parent, FormatEnum.MESSAGEML);

      case CashTag.MESSAGEML_TAG:
        validatePresentationML(tag);
        return new CashTag(++size, parent, FormatEnum.MESSAGEML);

      case Mention.MESSAGEML_TAG:
        validatePresentationML(tag);
        return new Mention(++size, parent, dataProvider, FormatEnum.MESSAGEML);

      case Link.MESSAGEML_TAG:
        return new Link(++size, parent, dataProvider);

      case Image.MESSAGEML_TAG:
        return new Image(++size, parent);

      case BulletList.MESSAGEML_TAG:
        return new BulletList(++size, parent);

      case OrderedList.MESSAGEML_TAG:
        return new OrderedList(++size, parent);

      case ListItem.MESSAGEML_TAG:
        return new ListItem(++size, parent);

      case Table.MESSAGEML_TAG:
        return new Table(++size, parent);

      case TableHeader.MESSAGEML_TAG:
        return new TableHeader(++size, parent);

      case TableBody.MESSAGEML_TAG:
        return new TableBody(++size, parent);

      case TableFooter.MESSAGEML_TAG:
        return new TableFooter(++size, parent);

      case TableRow.MESSAGEML_TAG:
        return new TableRow(++size, parent);

      case TableHeaderCell.MESSAGEML_TAG:
        return new TableHeaderCell(++size, parent);

      case TableCell.MESSAGEML_TAG:
        return new TableCell(++size, parent);

      case Card.MESSAGEML_TAG:
        validatePresentationML(tag);
        return new Card(++size, parent, FormatEnum.MESSAGEML);

      case CardHeader.MESSAGEML_TAG:
        validatePresentationML(tag);
        return new CardHeader(++size, parent, FormatEnum.MESSAGEML);

      case CardBody.MESSAGEML_TAG:
        validatePresentationML(tag);
        return new CardBody(++size, parent, FormatEnum.MESSAGEML);

      default:
        throw new InvalidInputException("Invalid MessageML content at element \"" + tag + "\"");
    }
  }

  /**
   * Return the number of elements in this context's MessageML tree.
   */
  public int getSize() {
    return size;
  }

  private Element createEntity(org.w3c.dom.Element element, Element parent) throws InvalidInputException {
    JsonNode entity = entityJson.path(element.getAttribute(Entity.ENTITY_ID_ATTR));
    JsonNode type = entity.path(Entity.TYPE_FIELD);
    JsonNode value = entity.path(Entity.ID_FIELD).path(0).path(Entity.VALUE_FIELD);

    if (entity.isMissingNode()) {
      throw new InvalidInputException("The attribute \"data-entity-id\" is required");
    }

    if (type.isMissingNode() || value.isMissingNode()) {
      throw new InvalidInputException("Entity data not found for node \"" + entity + "\"");
    }

    switch (type.textValue()) {
      case CashTag.ENTITY_TYPE:
        return new CashTag(++size, parent, value.textValue(), FormatEnum.PRESENTATIONML);
      case HashTag.ENTITY_TYPE:
        return new HashTag(++size, parent, value.textValue(), FormatEnum.PRESENTATIONML);
      case Mention.ENTITY_TYPE:
        return new Mention(++size, parent, value.longValue(), dataProvider, FormatEnum.PRESENTATIONML);
      default:
        return new Span(++size, parent);
    }
  }

}

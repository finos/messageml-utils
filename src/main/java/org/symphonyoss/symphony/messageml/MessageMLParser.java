package org.symphonyoss.symphony.messageml;

import static org.symphonyoss.symphony.messageml.elements.Element.CLASS_ATTR;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import freemarker.core.TemplateClassResolver;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import org.apache.commons.io.input.ReaderInputStream;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.elements.Bold;
import org.symphonyoss.symphony.messageml.elements.BulletList;
import org.symphonyoss.symphony.messageml.elements.Button;
import org.symphonyoss.symphony.messageml.elements.Card;
import org.symphonyoss.symphony.messageml.elements.CardBody;
import org.symphonyoss.symphony.messageml.elements.CardHeader;
import org.symphonyoss.symphony.messageml.elements.CashTag;
import org.symphonyoss.symphony.messageml.elements.Checkbox;
import org.symphonyoss.symphony.messageml.elements.Chime;
import org.symphonyoss.symphony.messageml.elements.Code;
import org.symphonyoss.symphony.messageml.elements.DatePicker;
import org.symphonyoss.symphony.messageml.elements.DateSelector;
import org.symphonyoss.symphony.messageml.elements.Dialog;
import org.symphonyoss.symphony.messageml.elements.DialogChild;
import org.symphonyoss.symphony.messageml.elements.Div;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.Emoji;
import org.symphonyoss.symphony.messageml.elements.Entity;
import org.symphonyoss.symphony.messageml.elements.ExpandableCard;
import org.symphonyoss.symphony.messageml.elements.ExpandableCardBody;
import org.symphonyoss.symphony.messageml.elements.ExpandableCardHeader;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.FormElement;
import org.symphonyoss.symphony.messageml.elements.FormatEnum;
import org.symphonyoss.symphony.messageml.elements.HashTag;
import org.symphonyoss.symphony.messageml.elements.Header;
import org.symphonyoss.symphony.messageml.elements.HorizontalRule;
import org.symphonyoss.symphony.messageml.elements.Image;
import org.symphonyoss.symphony.messageml.elements.Italic;
import org.symphonyoss.symphony.messageml.elements.LabelableElement;
import org.symphonyoss.symphony.messageml.elements.LineBreak;
import org.symphonyoss.symphony.messageml.elements.Link;
import org.symphonyoss.symphony.messageml.elements.ListItem;
import org.symphonyoss.symphony.messageml.elements.Mention;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.Option;
import org.symphonyoss.symphony.messageml.elements.OrderedList;
import org.symphonyoss.symphony.messageml.elements.Paragraph;
import org.symphonyoss.symphony.messageml.elements.PersonSelector;
import org.symphonyoss.symphony.messageml.elements.Preformatted;
import org.symphonyoss.symphony.messageml.elements.Radio;
import org.symphonyoss.symphony.messageml.elements.Select;
import org.symphonyoss.symphony.messageml.elements.Span;
import org.symphonyoss.symphony.messageml.elements.SplittableElement;
import org.symphonyoss.symphony.messageml.elements.Table;
import org.symphonyoss.symphony.messageml.elements.TableBody;
import org.symphonyoss.symphony.messageml.elements.TableCell;
import org.symphonyoss.symphony.messageml.elements.TableFooter;
import org.symphonyoss.symphony.messageml.elements.TableHeader;
import org.symphonyoss.symphony.messageml.elements.TableHeaderCell;
import org.symphonyoss.symphony.messageml.elements.TableRow;
import org.symphonyoss.symphony.messageml.elements.TextArea;
import org.symphonyoss.symphony.messageml.elements.TextField;
import org.symphonyoss.symphony.messageml.elements.TimePicker;
import org.symphonyoss.symphony.messageml.elements.TimezonePicker;
import org.symphonyoss.symphony.messageml.elements.TooltipableElement;
import org.symphonyoss.symphony.messageml.elements.UIAction;
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
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

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
 */
public class MessageMLParser {
  private static final ObjectMapper MAPPER = new ObjectMapper();
  private static final Configuration FREEMARKER = new Configuration(Configuration.VERSION_2_3_30);

  // Store XML factories as thread locals as they are costly to create.
  // Sonar warnings are ignored, we favor speed over memory usage, factories will stay in active threads
  @SuppressWarnings("java:S5164")
  private static final ThreadLocal<XPathFactory> X_PATH_FACTORY = ThreadLocal.withInitial(XPathFactory::newInstance);

  @SuppressWarnings("java:S5164")
  private static final ThreadLocal<DocumentBuilderFactory> DB_FACTORY = ThreadLocal.withInitial(() -> {
    try {
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      // XXE prevention as per https://www.owasp.org/index.php/XML_External_Entity_(XXE)_Prevention_Cheat_Sheet
      dbFactory.setXIncludeAware(false);
      dbFactory.setExpandEntityReferences(false);
      dbFactory.setIgnoringElementContentWhitespace(true);
      dbFactory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
      dbFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
      return dbFactory;
    } catch (ParserConfigurationException e) {
      throw new RuntimeException(e); //NOSONAR
    }
  });

  private final IDataProvider dataProvider;

  private BiContext biContext;
  private FormatEnum messageFormat;
  private ObjectNode entityJson;

  private int index;

  private Set<String> elementIds;
  // Map for storing SplittableElements components. The key is the id, the value the data holder of attributes
  private Map<String, SplittableData> splittableComponents;

  static {
    FREEMARKER.setDefaultEncoding("UTF-8");
    FREEMARKER.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
    FREEMARKER.setLogTemplateExceptions(false);
    FREEMARKER.setNewBuiltinClassResolver(TemplateClassResolver.ALLOWS_NOTHING_RESOLVER);
  }

  MessageMLParser(IDataProvider dataProvider) {
    this.dataProvider = dataProvider;
  }

  /**
   * Parse the text contents of the message and optionally EntityJSON into a MessageMLV2 message. Expands
   * Freemarker templates and generates a MessageML document tree.
   *
   * @param message    string containing a MessageMLV2 message with optional Freemarker templates
   * @param entityJson string containing EntityJSON data
   * @param version    string containing the version of the message format
   * @throws InvalidInputException thrown on invalid MessageMLV2 input
   * @throws ProcessingException   thrown on errors generating the document tree
   */
  MessageML parse(String message, String entityJson, String version) throws InvalidInputException, ProcessingException {
    clearBiContext();
    this.index = 0;
    this.elementIds = new HashSet<>();
    this.splittableComponents = new HashMap<>();
    String expandedMessage;

    if (StringUtils.isBlank(message)) {
      throw new InvalidInputException("Error parsing message: the message cannot be null or empty");
    }

    parseEntityJson(entityJson);

    try {
      expandedMessage = expandTemplates(message, this.entityJson);
    } catch (IOException e) {
      throw new InvalidInputException("Error parsing EntityJSON: " + e.getMessage());
    } catch (TemplateException e) {
      throw new InvalidInputException(String.format("Error parsing Freemarker template: invalid input at line %s, "
          + "column %s", e.getLineNumber(), e.getColumnNumber()));
    }

    MessageML messageML = parseMessageML(expandedMessage, version);
    this.entityJson = messageML.asEntityJson(this.entityJson);
    this.biContext.addItemWithValue(BiFields.MESSAGE_LENGTH.getValue(), message.length());
    return messageML;
  }

  private void parseEntityJson(String entityJson) throws InvalidInputException {
    if (StringUtils.isNotBlank(entityJson)) {
      this.biContext.addItemWithValue(BiFields.ENTITY_JSON_SIZE.getValue(), entityJson.length());
      try {
        JsonNode jsonNode = MAPPER.readTree(entityJson);
        if (jsonNode.isObject()) {
          this.entityJson = (ObjectNode) jsonNode;
        } else if (jsonNode.isTextual()) {
          // we got text and not a structured object, try to parse it as escaped JSON
          jsonNode = MAPPER.readTree(jsonNode.asText());
          if (jsonNode.isObject()) {
            this.entityJson = (ObjectNode) jsonNode;
          } else {
            throw new InvalidInputException("Error parsing EntityJSON: provided content is not a JSON object");
          }
        } else {
          throw new InvalidInputException("Error parsing EntityJSON: provided content is not a JSON object");
        }
      } catch (JsonProcessingException e) {
        throw new InvalidInputException("Error parsing EntityJSON: " + e.getMessage());
      }
    } else {
      this.entityJson = new ObjectNode(JsonNodeFactory.instance);
    }
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
  private static void validateEntities(String messageML, org.w3c.dom.Element document, JsonNode entityJson)
      throws InvalidInputException, ProcessingException {
    // quick bypass to avoid xpath evaluation if possible
    if (!messageML.contains("data-entity-id")) {
      return;
    }

    XPath xpath = X_PATH_FACTORY.get().newXPath();
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

      JsonNode entityNode = entityJson.findPath(entityId);
      if (entityNode.isMissingNode()) {
        throw new InvalidInputException("Error processing EntityJSON: "
            + "no entity data provided for \"data-entity-id\"=\"" + entityId + "\"");
      } else if (!entityNode.isObject()) {
        throw new InvalidInputException("Error processing EntityJSON: "
            + "the node \"" + entityId + "\" has to be an object");
      }
    }
  }

  /**
   * Throw an exception if the enclosing message is in PresentationML and a MessageML tag is used.
   */
  private void validateFormat(String tag) throws InvalidInputException {
    if (messageFormat == FormatEnum.PRESENTATIONML) {
      throw new InvalidInputException("Shorthand tag \"" + tag + "\" is not allowed in PresentationML");
    }
  }

  /**
   * Expand Freemarker templates.
   */
  private String expandTemplates(String message, JsonNode entityJson) throws IOException, TemplateException {
    // quick bypass to avoid creating the templating engine if possible
    if (!containsFreemarkerTags(message)) {
      return message;
    }

    // Read entityJSON data
    Map<String, Object> data = new HashMap<>();
    data.put("data", MAPPER.convertValue(entityJson, Map.class));
    data.put("entity", MAPPER.convertValue(entityJson, Map.class));

    // Read MessageMLV2 template
    StringWriter sw = new StringWriter();
    Template template = new Template("messageML", message, FREEMARKER);

    // Expand the template
    template.process(data, sw);

    if (sw.toString().length() != message.length()) {
      this.biContext.updateItemCount(BiFields.FREEMARKER.getValue());
    }
    return sw.toString();
  }

  private boolean containsFreemarkerTags(String message) {
    // Based https://freemarker.apache.org/docs/dgui_template_directives.html
    // We consider that directives cannot be customized (to use [ or without #)
    return message.contains("<#") || message.contains("<@") || message.contains("${") || message.contains("#{");
  }

  /**
   * Parse the message string into its MessageML representation.
   */
  private MessageML parseMessageML(String messageML, String version) throws InvalidInputException, ProcessingException {
    validateMessageText(messageML);

    org.w3c.dom.Element docElement = parseDocument(messageML);

    validateEntities(messageML, docElement, entityJson);

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
  org.w3c.dom.Element parseDocument(String messageML) throws InvalidInputException, ProcessingException {
    try {
      DocumentBuilder dBuilder = DB_FACTORY.get().newDocumentBuilder();
      dBuilder.setErrorHandler(new NullErrorHandler()); // default handler prints to stderr
      dBuilder.setEntityResolver(new NoOpEntityResolver());

      StringReader sr = new StringReader(messageML);
      ReaderInputStream ris = new ReaderInputStream(sr, StandardCharsets.UTF_8);

      Document doc = dBuilder.parse(ris);

      doc.getDocumentElement().normalize();

      return doc.getDocumentElement();

    } catch (SAXException e) {
      throw new InvalidInputException("Invalid messageML: " + e.getMessage(), e);
    } catch (ParserConfigurationException | IOException e) {
      throw new ProcessingException("Failed to parse messageML", e);
    }
  }

  private boolean containsAttribute(String input, String attribute) {
    String[] splitInput = input.split("\\s+");

    return Arrays.asList(splitInput).contains(attribute);
  }

  private void removeAttribute(org.w3c.dom.Element element, String input, String attribute) {
    if (element.hasAttribute(input)) {
      String newAttribute = Arrays.stream(element.getAttribute(input).split("\\s+"))
          .filter(it -> !it.equalsIgnoreCase(attribute))
          .collect(Collectors.joining(" "));

      if (StringUtils.isNotBlank(newAttribute)) {
        element.setAttribute(input, newAttribute);
      } else {
        element.removeAttribute(input);
      }
    }
  }

  /**
   * Create a MessageML element based on the DOM element's name and attributes.
   * It can return null when the element must be not created (e.g. see label element {@link LabelableElement} or tooltip
   * element {@link TooltipableElement}) that is PresentationML specific, but in MessageML is not an element,
   * instead it is an attribute of another element) and it should be not considered an error; in case of a real error,
   * an exception is thrown
   */
  public Element createElement(org.w3c.dom.Element element, Element parent) throws
      InvalidInputException {
    String tag = element.getNodeName();

    if (Header.isHeaderElement(tag)) {
      return new Header(parent, tag);
    }

    String elementClass = element.getAttribute(CLASS_ATTR);
    switch (tag) {
      case Chime.MESSAGEML_TAG:
        validateFormat(tag);
        return new Chime(parent, FormatEnum.MESSAGEML);

      case Chime.PRESENTATIONML_TAG:
        return new Chime(parent, FormatEnum.PRESENTATIONML);

      case Paragraph.MESSAGEML_TAG:
        return new Paragraph(parent);

      case LineBreak.MESSAGEML_TAG:
        return new LineBreak(parent);

      case HorizontalRule.MESSAGEML_TAG:
        return new HorizontalRule(parent);

      case Span.MESSAGEML_TAG:
        if (TooltipableElement.isTooltipNode(element)) {
          String id = getAttribute(element, TooltipableElement.DATA_TARGET_ID);
          createSplittable(id, TooltipableElement.class, null);
          String title = getAttribute(element, TooltipableElement.DATA_TITLE);
          addSplittableData(id, TooltipableElement.class, TooltipableElement.TITLE, title);
          return null;
        } else if (containsAttribute(elementClass, Entity.PRESENTATIONML_CLASS)) {
          return createEntity(element, parent);
        } else {
          return new Span(parent);
        }

      case Div.MESSAGEML_TAG:
        return createElementFromDiv(element, parent);

      case FormElement.INPUT_TAG:
        return createElementFromInput(element, parent);

      case Bold.MESSAGEML_TAG:
        return new Bold(parent);

      case Italic.MESSAGEML_TAG:
        return new Italic(parent);

      case Preformatted.MESSAGEML_TAG:
        return new Preformatted(parent);

      case HashTag.MESSAGEML_TAG:
        validateFormat(tag);
        return new HashTag(parent, ++index);

      case CashTag.MESSAGEML_TAG:
        validateFormat(tag);
        return new CashTag(parent, ++index);

      case Mention.MESSAGEML_TAG:
        validateFormat(tag);
        return new Mention(parent, ++index, dataProvider);

      case Link.MESSAGEML_TAG:
        return new Link(parent, dataProvider);

      case Image.MESSAGEML_TAG:
        return new Image(parent);

      case BulletList.MESSAGEML_TAG:
        return new BulletList(parent);

      case OrderedList.MESSAGEML_TAG:
        return new OrderedList(parent);

      case ListItem.MESSAGEML_TAG:
        return new ListItem(parent);

      case Table.MESSAGEML_TAG:
        return new Table(parent);

      case TableHeader.MESSAGEML_TAG:
        return new TableHeader(parent);

      case TableBody.MESSAGEML_TAG:
        return new TableBody(parent);

      case TableFooter.MESSAGEML_TAG:
        return new TableFooter(parent);

      case TableRow.MESSAGEML_TAG:
        return new TableRow(parent);

      case TableHeaderCell.MESSAGEML_TAG:
        return new TableHeaderCell(parent);

      case TableCell.MESSAGEML_TAG:
        return new TableCell(parent);

      case Card.MESSAGEML_TAG:
        validateFormat(tag);
        return new Card(parent, messageFormat);

      case ExpandableCard.MESSAGEML_TAG:
        validateFormat(tag);
        return new ExpandableCard(parent, messageFormat);

      case Code.MESSAGEML_TAG:
        return new Code(parent);

      case CardHeader.MESSAGEML_TAG:
        validateFormat(tag);
        if (parent instanceof ExpandableCard) {
          return new ExpandableCardHeader(parent, messageFormat);
        } else {
          return new CardHeader(parent, messageFormat);
        }

      case CardBody.MESSAGEML_TAG:
        validateFormat(tag);
        if (parent instanceof Dialog || parent.getParent() instanceof Dialog) {
          return new DialogChild.Body(parent, messageFormat);
        } else if (parent instanceof ExpandableCard) {
          return new ExpandableCardBody(parent, messageFormat);
        } else {
          return new CardBody(parent, messageFormat);
        }

      case Emoji.MESSAGEML_TAG:
        return new Emoji(parent, ++index);

      case Form.MESSAGEML_TAG:
        return new Form(parent, messageFormat);

      case Select.MESSAGEML_TAG:
        return new Select(parent);

      case Option.MESSAGEML_TAG:
        return new Option(parent);

      case Button.MESSAGEML_TAG:
        return new Button(parent, messageFormat);

      case TextField.MESSAGEML_TAG:
        return new TextField(parent, messageFormat);

      case Checkbox.MESSAGEML_TAG:
        return new Checkbox(parent, messageFormat);

      case Radio.MESSAGEML_TAG:
        return new Radio(parent, messageFormat);

      case PersonSelector.MESSAGEML_TAG:
        return new PersonSelector(parent, messageFormat);

      case DateSelector.MESSAGEML_TAG:
        return new DateSelector(parent, messageFormat);

      case DatePicker.MESSAGEML_TAG:
        return new DatePicker(parent, messageFormat);

      case TimePicker.MESSAGEML_TAG:
        return new TimePicker(parent, messageFormat);

      case TimezonePicker.MESSAGEML_TAG:
        return new TimezonePicker(parent, messageFormat);

      case TextArea.MESSAGEML_TAG:
        return new TextArea(parent, messageFormat);

      case UIAction.MESSAGEML_TAG:
        return new UIAction(parent, messageFormat);

      case Dialog.MESSAGEML_TAG:
        return new Dialog(parent, messageFormat);

      case DialogChild.Title.MESSAGEML_TAG:
        return new DialogChild.Title(parent, messageFormat);

      case DialogChild.Footer.MESSAGEML_TAG:
        return new DialogChild.Footer(parent, messageFormat);

      case LabelableElement.LABEL:
        String id = getAttribute(element, LabelableElement.LABEL_FOR);
        createSplittable(id, LabelableElement.class, Pair.of(LabelableElement.LABEL, element.getTextContent()));
        return null;

      default:
        throw new InvalidInputException("Invalid MessageML content at element \"" + tag + "\"");
    }
  }

  private String getAttribute(org.w3c.dom.Element element, String key)
      throws InvalidInputException {
    String value = element.getAttribute(key);
    if (value == null || value.isEmpty()) {
      throw new InvalidInputException(
          String.format("Invalid MessageML content at element \"%s\": 'data-target-id' attribute missing or empty",
              key));
    }
    if (splittableContains(value, TooltipableElement.class)) {
      throw new InvalidInputException(
          String.format("Invalid MessageML content at element \"%s\": 'data-target-id' value already existing", key));
    }
    return value;
  }

  /**
   * Loads the values of the "id" attribute of elements being parsed and verifies if these values are unique.
   *
   * @param id
   * @throws InvalidInputException
   */
  public void loadElementId(String id) throws InvalidInputException {
    if (!elementIds.add(id)) {
      throw new InvalidInputException(
          String.format("Elements must have unique ids. The following value is not unique: [%s].", id));
    }
  }

  /**
   * Returns the attributes corresponding to the id for a splittable element
   * (used internally during parsing)
   */
  public Optional<Map<String, String>> getSplittableAttributes(String id, Class<? extends SplittableElement> clazz)
      throws InvalidInputException {
    SplittableData data = splittableComponents.get(id);
    if (data == null) {
      return Optional.empty();
    }
    return data.getAttributes(clazz);
  }

  /**
   * Returns all attributes corresponding to the id
   * (used internally during parsing)
   */
  public Optional<Map<Class<? extends SplittableElement>, Map<String, String>>> getAllSplittableAttributes(String id)
      throws InvalidInputException {
    SplittableData data = splittableComponents.get(id);
    if (data == null) {
      return Optional.empty();
    }
    return Optional.of(data.getAllAttributes());
  }

  /**
   * Returns all values corresponding to the id
   * (used internally during parsing)
   */
  public Optional<Map<Class<? extends SplittableElement>, Pair<String, String>>> getAllSplittableValues(String id)
      throws InvalidInputException {
    SplittableData data = splittableComponents.get(id);
    if (data == null) {
      return Optional.empty();
    }
    return Optional.of(data.getAllValues());
  }

  private void createSplittable(String id, Class<? extends SplittableElement> clazz, Pair<String, String> value)
      throws InvalidInputException {
    SplittableData data = splittableComponents.get(id);
    if (data == null) {
      data = new SplittableData();
      splittableComponents.put(id, data);
    }
    data.create(clazz, value);
  }

  /**
   * Add a splittable element value in the map
   */
  private void addSplittableData(String id, Class<? extends SplittableElement> clazz, String attributeName,
      String attributeValue) {
    SplittableData data = splittableComponents.get(id);
    if (data == null) {
      data = new SplittableData();
      splittableComponents.put(id, data);
    }
    data.addAttribute(clazz, attributeName, attributeValue);
  }

  /**
   * Checks if the splittable map contains the element
   */
  private boolean splittableContains(String id, Class<? extends SplittableElement> clazz) {
    SplittableData data = splittableComponents.get(id);
    if (data != null) {
      return data.exists(clazz);
    }
    return false;
  }

  private Element createElementFromInput(org.w3c.dom.Element element, Element parent) throws InvalidInputException {
    String elementType = element.getAttribute(FormElement.TYPE_ATTR);

    if (containsAttribute(elementType, TextField.PRESENTATIONML_INPUT_TYPE)) {
      removeAttribute(element, FormElement.TYPE_ATTR, TextField.PRESENTATIONML_INPUT_TYPE);
      return new TextField(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementType, Checkbox.PRESENTATIONML_INPUT_TYPE)) {
      return new Checkbox(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementType, Radio.PRESENTATIONML_INPUT_TYPE)) {
      return new Radio(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementType, DatePicker.PRESENTATIONML_INPUT_TYPE)) {
      return new DatePicker(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementType, TimePicker.PRESENTATIONML_INPUT_TYPE)) {
      return new TimePicker(parent, FormatEnum.PRESENTATIONML);
    } else {
      throw new InvalidInputException(
          String.format("The input type \"%s\" is not allowed on PresentationML", elementType));
    }
  }

  private Element createElementFromDiv(org.w3c.dom.Element element, Element parent) throws InvalidInputException {
    if (element.hasAttribute(SplittableElement.PRESENTATIONML_DIV_FLAG)) {
      // Special div created by a splittable element, it is not converted in MessageML or in PresentationML tree object
      return null;
    }

    String elementClass = element.getAttribute(CLASS_ATTR);
    if (containsAttribute(elementClass, Entity.PRESENTATIONML_CLASS)) {
      return createEntity(element, parent);
    } else if (containsAttribute(elementClass, Card.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, Card.PRESENTATIONML_CLASS);
      return new Card(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, CardBody.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, CardBody.PRESENTATIONML_CLASS);
      return new CardBody(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, CardHeader.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, CardHeader.PRESENTATIONML_CLASS);
      return new CardHeader(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, ExpandableCard.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, ExpandableCard.PRESENTATIONML_CLASS);
      return new ExpandableCard(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, ExpandableCardBody.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, ExpandableCardBody.PRESENTATIONML_CLASS);
      return new ExpandableCardBody(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, ExpandableCardHeader.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, ExpandableCardHeader.PRESENTATIONML_CLASS);
      return new ExpandableCardHeader(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, PersonSelector.MESSAGEML_TAG)) {
      removeAttribute(element, CLASS_ATTR, PersonSelector.MESSAGEML_TAG);
      return new PersonSelector(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, DateSelector.MESSAGEML_TAG)) {
      removeAttribute(element, CLASS_ATTR, DateSelector.MESSAGEML_TAG);
      return new DateSelector(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, Checkbox.PRESENTATIONML_DIV_CLASS)) {
      removeAttribute(element, CLASS_ATTR, Checkbox.PRESENTATIONML_DIV_CLASS);
      return new Checkbox(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, Radio.PRESENTATIONML_DIV_CLASS)) {
      removeAttribute(element, CLASS_ATTR, Radio.PRESENTATIONML_DIV_CLASS);
      return new Radio(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, UIAction.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, UIAction.PRESENTATIONML_CLASS);
      return new UIAction(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, TimezonePicker.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, TimezonePicker.PRESENTATIONML_CLASS);
      return new TimezonePicker(parent, FormatEnum.PRESENTATIONML);
    } else if (containsAttribute(elementClass, Dialog.PRESENTATIONML_CLASS)) {
      removeAttribute(element, CLASS_ATTR, Dialog.PRESENTATIONML_CLASS);
      return new Dialog(parent, FormatEnum.PRESENTATIONML);
    } else {
      return new Div(parent);
    }
  }

  private Element createEntity(org.w3c.dom.Element element, Element parent) throws InvalidInputException {
    String entityId = element.getAttribute(Entity.ENTITY_ID_ATTR);
    String tag = element.getNodeName();
    List<JsonNode> entityList = entityJson.findValues(entityId);

    if (entityList.isEmpty()) {
      throw new InvalidInputException("The attribute \"data-entity-id\" is required");
    } else if (entityList.size() > 1) {
      throw new InvalidInputException("Duplicate \"data-entity-id\"=\"" + entityId + "\" in entityJSON");
    }

    JsonNode entity = entityList.get(0);
    JsonNode type = entity.path(Entity.TYPE_FIELD);
    JsonNode value = entity.path(Entity.ID_FIELD).path(0).path(Entity.VALUE_FIELD);

    if (!type.isMissingNode() && !value.isMissingNode()) {
      switch (type.textValue()) {
        case CashTag.ENTITY_TYPE:
          return new CashTag(parent, tag, value.asText());
        case HashTag.ENTITY_TYPE:
          return new HashTag(parent, tag, value.asText());
        case Mention.ENTITY_TYPE:
          return new Mention(parent, tag, value.asLong(), dataProvider);
        default:
          break;
      }
    }

    if (Div.MESSAGEML_TAG.equals(tag)) {
      return new Div(parent);
    } else if (Span.MESSAGEML_TAG.equals(tag)) {
      return new Span(parent);
    } else {
      throw new InvalidInputException("The element \"" + tag + "\" cannot be an entity");
    }
  }

  public FormatEnum getMessageFormat() {
    return messageFormat;
  }

  public BiContext getBiContext() {
    return this.biContext;
  }

  public void clearBiContext() {
    this.biContext = new BiContext();
  }

  /**
   * Internal class for storing data about splittable elements during parse
   */
  static final class SplittableData {

    private final Map<Class<? extends SplittableElement>, Pair<String, String>> valuesBySplittable =
        new LinkedHashMap<>();
    private final Map<Class<? extends SplittableElement>, Map<String, String>> attributesBySplittable =
        new LinkedHashMap<>();

    public void create(Class<? extends SplittableElement> splittable, Pair<String, String> value)
        throws InvalidInputException {
      if (valuesBySplittable.containsKey(splittable)) {
        throw new InvalidInputException(
            "Invalid MessageML content, multiple splittable elements with the same id created");
      }
      if (value != null) {
        valuesBySplittable.put(splittable, value);
      }
      attributesBySplittable.put(splittable, new LinkedHashMap<>());
    }

    public boolean exists(Class<? extends SplittableElement> splittable) {
      return valuesBySplittable.containsKey(splittable);
    }

    public Map<Class<? extends SplittableElement>, Pair<String, String>> getAllValues() {
      return new LinkedHashMap<>(valuesBySplittable);
    }

    public void addAttribute(Class<? extends SplittableElement> splittable, String attributeName,
        String attributeValue) {
      Map<String, String> attributes = attributesBySplittable.get(splittable);
      if (attributes == null) {
        throw new UnsupportedOperationException("Create new SplittableData before adding attributes");
      }
      attributes.put(attributeName, attributeValue);
    }

    public Optional<Map<String, String>> getAttributes(Class<? extends SplittableElement> splittable) {
      Map<String, String> attributes = attributesBySplittable.get(splittable);
      if (attributes == null) {
        return Optional.empty();
      }
      return Optional.of(new LinkedHashMap<>(attributes));
    }

    public Map<Class<? extends SplittableElement>, Map<String, String>> getAllAttributes() {
      return new LinkedHashMap<>(attributesBySplittable);
    }
  }
}

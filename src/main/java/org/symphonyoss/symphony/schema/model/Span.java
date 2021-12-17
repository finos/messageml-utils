package org.symphonyoss.symphony.schema.model;

import lombok.Data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * No formatting.
 * <ul>
 *   <li>This tag can be used to specify visual styles, by adding a class attribute.</li>
 *   <li>This tag is used to create Structured objects.</li>
 * </ul>
 */
@Data
@XmlRootElement(name = "span")
@XmlAccessorType(XmlAccessType.FIELD)
public class Span extends PhrasingContent {

  @XmlAttribute(name = "class")
  String classValue;

  @XmlAttribute(name = "style")
  String style;

  @XmlAttribute(name = "data-entity-id")
  String entityId;
}

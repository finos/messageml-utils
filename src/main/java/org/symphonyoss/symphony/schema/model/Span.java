package org.symphonyoss.symphony.schema.model;

import lombok.Data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

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

package org.symphonyoss.symphony.schema.model;

import lombok.Data;

import java.net.URI;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlValue;

@Data
@XmlRootElement(name = "a")
@XmlAccessorType(XmlAccessType.FIELD)
public class Link {

  @XmlAttribute(required = true)
  URI href;

  @XmlValue
  String value;
}

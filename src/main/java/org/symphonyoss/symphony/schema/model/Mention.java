package org.symphonyoss.symphony.schema.model;

import lombok.Data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

@Data
@XmlRootElement(name = "mention")
@XmlAccessorType(XmlAccessType.FIELD)
public class Mention {

  @XmlAttribute
  private String uid;

  @XmlAttribute
  private String email;
}

package org.symphonyoss.symphony.schema.model;

import lombok.Data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * An emoji: <a href="https://docs.developers.symphony.com/building-bots-on-symphony/messages/overview-of-messageml/messageml-basic-format-tags/emojis">List of emojis.</a>
 */
@Data
@XmlRootElement(name = "emoji")
@XmlAccessorType(XmlAccessType.FIELD)
public class Emoji {

  @XmlAttribute(required = true)
  private String shortcode;

}

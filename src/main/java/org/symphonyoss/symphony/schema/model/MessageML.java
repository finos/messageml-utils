package org.symphonyoss.symphony.schema.model;

import lombok.Data;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlMixed;
import javax.xml.bind.annotation.XmlRootElement;

@Data
@XmlRootElement(name = "messageML")
@XmlAccessorType(XmlAccessType.FIELD)
public class MessageML {

  @XmlMixed
  @XmlElementRefs({
      // Text formatting
      @XmlElementRef(type = Link.class),
      @XmlElementRef(type = Bold.class),
      @XmlElementRef(type = Italic.class),
      @XmlElementRef(type = Preformatted.class),
      @XmlElementRef(type = LineBreak.class),
      @XmlElementRef(type = HorizontalRule.class),
      @XmlElementRef(type = Span.class),
      @XmlElementRef(type = Mention.class),
      @XmlElementRef(type = HashTag.class),
      @XmlElementRef(type = CashTag.class),
      @XmlElementRef(type = Emoji.class),
      @XmlElementRef(type = Image.class),
      @XmlElementRef(type = Header.H1.class),
      @XmlElementRef(type = Header.H2.class),
      @XmlElementRef(type = Header.H3.class),
      @XmlElementRef(type = Header.H4.class),
      @XmlElementRef(type = Header.H5.class),
      @XmlElementRef(type = Header.H6.class),
      @XmlElementRef(type = Chime.class),
  })
  private List<Object> content;
}

package org.symphonyoss.symphony.schema.model;

import lombok.Data;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlMixed;

@Data
@XmlAccessorType(XmlAccessType.FIELD)
public abstract class PhrasingContent {

  @XmlMixed
  @XmlElementRefs({
      @XmlElementRef(type = Link.class),
      @XmlElementRef(type = Bold.class),
      @XmlElementRef(type = Italic.class),
      @XmlElementRef(type = LineBreak.class),
      @XmlElementRef(type = Span.class),
      @XmlElementRef(type = Mention.class),
      @XmlElementRef(type = HashTag.class),
      @XmlElementRef(type = CashTag.class),
      @XmlElementRef(type = Emoji.class),
      @XmlElementRef(type = Image.class),
      @XmlElementRef(type = Chime.class),
  })
  private List<Object> content;
}

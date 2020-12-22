package org.symphonyoss.symphony.messageml.util;

import java.io.Serializable;

/**
 * This class is used to wrap an XML attribute values processed by {@link org.symphonyoss.symphony.messageml.util.XmlPrintStream}
 * When passing an attribute, it is possible to use a string, however sometime is needed to specify the also the format, in this case
 * use this class
 *
 * @author enrico.molino (23/11/2020)
 */
public class XMLAttribute implements Serializable {
  public final String value;
  public final Format format;

  public enum Format { STANDARD, JSON }

  private XMLAttribute(String name, Format format) {
    this.value = name;
    this.format = format;
  }

  public static XMLAttribute of(String name, Format format){
    return new XMLAttribute(name, format == null ? Format.STANDARD : format);
  }

  public Format getFormat() {
    return format;
  }

  @Override
  public String toString() {
    return value;
  }
}

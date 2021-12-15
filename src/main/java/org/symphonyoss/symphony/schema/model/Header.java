package org.symphonyoss.symphony.schema.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class Header extends PhrasingContent {

  @XmlRootElement(name = "h1")
  public static class H1 extends Header {}

  @XmlRootElement(name = "h2")
  public static class H2 extends Header {}

  @XmlRootElement(name = "h3")
  public static class H3 extends Header {}

  @XmlRootElement(name = "h4")
  public static class H4 extends Header {}

  @XmlRootElement(name = "h5")
  public static class H5 extends Header {}

  @XmlRootElement(name = "h6")
  public static class H6 extends Header {}
}

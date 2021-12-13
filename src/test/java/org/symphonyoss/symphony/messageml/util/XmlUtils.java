package org.symphonyoss.symphony.messageml.util;

import org.w3c.dom.Node;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.InputSource;

import java.io.StringReader;

import javax.xml.parsers.DocumentBuilderFactory;

public class XmlUtils {

  public static String prettyXml(String input) {
    try {
      final InputSource src = new InputSource(new StringReader(input));
      final Node document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(src).getDocumentElement();
      final DOMImplementationRegistry registry = DOMImplementationRegistry.newInstance();
      final DOMImplementationLS impl = (DOMImplementationLS) registry.getDOMImplementation("LS");
      final LSSerializer writer = impl.createLSSerializer();
      writer.getDomConfig().setParameter("format-pretty-print", Boolean.TRUE);
      writer.getDomConfig().setParameter("xml-declaration", false);
      return writer.writeToString(document);
    } catch (Exception ex) {
      System.err.printf("Cannot prettify XML input: %s", ex.getMessage());
      return input;
    }
  }
}

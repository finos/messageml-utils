package org.symphonyoss.symphony.schema;

import org.symphonyoss.symphony.schema.model.MessageML;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

/**
 * This main class is used to generate the MessageML XML Schema under classpath:src/main/resources/mml.xsd location.
 * <p>
 * We might want to run this as part of the Maven build.
 */
public class GenerateSchema {

  public static void main(String[] args) throws Exception {

    JAXBContext.newInstance(MessageML.class).generateSchema(new SchemaOutputResolver() {

      @Override
      public Result createOutput(String namespaceURI, String suggestedFileName) {
        return new StreamResult("src/main/resources/mml.xsd");
      }
    });
  }
}

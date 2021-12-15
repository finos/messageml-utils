package org.symphonyoss.symphony.schema;

import org.symphonyoss.symphony.schema.model.MessageML;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

public class GenerateSchema {

  public static void main(String[] args) throws Exception {
    JAXBContext jc = JAXBContext.newInstance(MessageML.class);
    jc.generateSchema(new SchemaOutputResolver() {

      @Override
      public Result createOutput(String namespaceURI, String suggestedFileName) {
        return new StreamResult("mml.xsd");
      }
    });
  }
}

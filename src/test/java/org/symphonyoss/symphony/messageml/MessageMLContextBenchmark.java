package org.symphonyoss.symphony.messageml;

import org.apache.commons.io.IOUtils;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.infra.Blackhole;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.NoOpDataProvider;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class MessageMLContextBenchmark {

  @State(Scope.Thread)
  public static class MessageContent {
    public String messageML;
    public String entityJson;

    @Setup(Level.Trial)
    public void doSetup() throws IOException {
      FileInputStream messageFile =
          new FileInputStream("src/test/resources/payloads/complex_message_with_styles.messageml");
      messageML = IOUtils.toString(messageFile, StandardCharsets.UTF_8);
      FileInputStream entityFile = new FileInputStream("src/test/resources/payloads/complex_message_with_styles.json");
      entityJson = IOUtils.toString(entityFile, StandardCharsets.UTF_8);
    }
  }

  @Benchmark
  public void parseSimpleMessageML(Blackhole bh) throws InvalidInputException, ProcessingException, IOException {
    MessageMLContext messageMLContext = new MessageMLContext(new NoOpDataProvider());
    messageMLContext.parseMessageML("<messageML>Hello</messageML>", "", null);

    // those calls are usually made by the agent when sending a message
    bh.consume(messageMLContext.getText());
    bh.consume(messageMLContext.getPresentationML());
  }

  @Benchmark
  public void parseComplexMessageMLWithEntities(MessageContent messageContent, Blackhole bh)
      throws InvalidInputException, ProcessingException, IOException {
    MessageMLContext messageMLContext = new MessageMLContext(new NoOpDataProvider());
    messageMLContext.parseMessageML(messageContent.messageML, messageContent.entityJson, null);

    // those calls are usually made by the agent when sending a message
    bh.consume(messageMLContext.getText());
    bh.consume(messageMLContext.getPresentationML());
  }
}

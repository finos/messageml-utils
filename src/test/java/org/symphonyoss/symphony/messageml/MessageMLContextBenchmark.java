package org.symphonyoss.symphony.messageml;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.infra.Blackhole;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.NoOpDataProvider;

import java.io.IOException;

public class MessageMLContextBenchmark {

  @Benchmark
  public void parseSimpleMessageML(Blackhole bh) throws InvalidInputException, ProcessingException, IOException {
    MessageMLContext messageMLContext = new MessageMLContext(new NoOpDataProvider());
    messageMLContext.parseMessageML("<messageML>Hello</messageML>", "", null);

    // those calls are usually made by the agent when sending a message
    bh.consume(messageMLContext.getText());
    bh.consume(messageMLContext.getPresentationML());
  }
}

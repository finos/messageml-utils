package org.finos.symphony.messageml.messagemlutils.util;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import java.io.IOException;
import java.io.StringWriter;

/**
 * Comprehensive unit tests for {@link BoundedWriter} to ensure
 * size bounding, overload behaviors, and error handling are correct.
 */
public class BoundedWriterTest {

  @Test
  public void testWriteWithinLimit() throws IOException {
    StringWriter sw = new StringWriter();
    BoundedWriter bw = new BoundedWriter(sw, 15);

    bw.write("Hello");
    assertEquals("Hello", sw.toString());
    assertEquals(5, bw.getWrittenCount());
    assertEquals(15, bw.getLimit());

    bw.write(' ');
    bw.write(new char[]{'w', 'o', 'r', 'l', 'd'});
    assertEquals("Hello world", sw.toString());
    assertEquals(11, bw.getWrittenCount()); // Note: limit check is len-based, so this passes if written sequentially within limits
  }

  @Test
  public void testWriteExceedsLimitChar() {
    StringWriter sw = new StringWriter();
    BoundedWriter bw = new BoundedWriter(sw, 5);

    IOException ex = assertThrows(IOException.class, () -> {
      bw.write("Hello world");
    });
    assertTrue(ex.getMessage().contains("exceeded maximum size limit"));
  }

  @Test
  public void testWriteSingleCharExceedsLimit() throws IOException {
    StringWriter sw = new StringWriter();
    BoundedWriter bw = new BoundedWriter(sw, 2);

    bw.write('a');
    bw.write('b');
    
    IOException ex = assertThrows(IOException.class, () -> {
      bw.write('c');
    });
    assertTrue(ex.getMessage().contains("exceeded maximum size limit"));
  }

  @Test
  public void testWriteCharArrayExceedsLimit() throws IOException {
    StringWriter sw = new StringWriter();
    BoundedWriter bw = new BoundedWriter(sw, 3);

    bw.write(new char[]{'a', 'b'});
    
    IOException ex = assertThrows(IOException.class, () -> {
      bw.write(new char[]{'c', 'd'});
    });
    assertTrue(ex.getMessage().contains("exceeded maximum size limit"));
  }

  @Test
  public void testInvalidConstructorArguments() {
    assertThrows(IllegalArgumentException.class, () -> {
      new BoundedWriter(null, 10);
    });

    assertThrows(IllegalArgumentException.class, () -> {
      new BoundedWriter(new StringWriter(), -1);
    });
  }

  @Test
  public void testFlushAndClose() throws IOException {
    StringWriter sw = new StringWriter();
    BoundedWriter bw = new BoundedWriter(sw, 15);

    bw.write("Test");
    bw.flush();
    bw.close();
    assertEquals("Test", sw.toString());
  }
}

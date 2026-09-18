package org.finos.symphony.messageml.messagemlutils.util;

import java.io.IOException;
import java.io.Writer;

/**
 * A security-hardening {@link Writer} decorator that caps the total number of characters
 * written to an underlying output stream.
 * <p>
 * This class is primarily intended to prevent Denial of Service (DoS) attacks during
 * template expansion (e.g. FreeMarker) where malicious or buggy infinite/huge loops
 * could generate massive payloads in memory.
 * </p>
 * <p>
 * Thread-safety: This class is not thread-safe. Multiple threads should not share the
 * same instance of {@link BoundedWriter} without external synchronization.
 * </p>
 */
public class BoundedWriter extends Writer {

  private final Writer target;
  private final int limit;
  private int written = 0;

  /**
   * Constructs a new {@code BoundedWriter} wrapping the specified target writer.
   *
   * @param target the underlying writer to receive the bounded output, must not be null
   * @param limit the maximum number of characters allowed to be written (inclusive)
   * @throws IllegalArgumentException if limit is negative or target is null
   */
  public BoundedWriter(Writer target, int limit) {
    if (target == null) {
      throw new IllegalArgumentException("Target writer must not be null");
    }
    if (limit < 0) {
      throw new IllegalArgumentException("Limit must be a non-negative integer");
    }
    this.target = target;
    this.limit = limit;
  }

  /**
   * Returns the total number of characters successfully processed by this writer so far.
   *
   * @return the number of characters written
   */
  public int getWrittenCount() {
    return this.written;
  }

  /**
   * Returns the maximum character limit.
   *
   * @return the write limit
   */
  public int getLimit() {
    return this.limit;
  }

  @Override
  public void write(char[] cbuf, int off, int len) throws IOException {
    checkLimit(len);
    target.write(cbuf, off, len);
    written += len;
  }

  @Override
  public void write(int c) throws IOException {
    checkLimit(1);
    target.write(c);
    written += 1;
  }

  @Override
  public void write(String str, int off, int len) throws IOException {
    checkLimit(len);
    target.write(str, off, len);
    written += len;
  }

  @Override
  public void flush() throws IOException {
    target.flush();
  }

  @Override
  public void close() throws IOException {
    target.close();
  }

  private void checkLimit(int len) throws IOException {
    if (written + len > limit) {
      throw new IOException("Template expansion exceeded maximum size limit of " + limit + " characters.");
    }
  }
}

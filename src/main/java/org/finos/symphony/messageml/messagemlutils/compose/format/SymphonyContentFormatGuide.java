/*
 * Copyright 2026 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.finos.symphony.messageml.messagemlutils.compose.format;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;

/**
 * The authoring guide for native {@code <messageML>} blocks inside extended Markdown, loaded once from
 * a classpath resource next to this class. Any producer can show it to authors or serve it on demand.
 */
public final class SymphonyContentFormatGuide {

  private static final String RESOURCE =
      "/org/finos/symphony/messageml/messagemlutils/compose/format/symphony-content-format-guide.md";
  private static final String TEXT = load(RESOURCE);

  private SymphonyContentFormatGuide() {
  }

  /** The guide text: when to use a block, the block rules, supported elements and what is not allowed. */
  public static String text() {
    return TEXT;
  }

  static String load(String resource) {
    try (InputStream in = SymphonyContentFormatGuide.class.getResourceAsStream(resource)) {
      if (in == null) {
        throw new IllegalStateException("Missing resource: " + resource);
      }
      return readAll(in);
    } catch (IOException e) {
      throw new UncheckedIOException("Failed to load " + resource, e);
    }
  }

  static String readAll(InputStream in) throws IOException {
    return new String(in.readAllBytes(), StandardCharsets.UTF_8);
  }
}

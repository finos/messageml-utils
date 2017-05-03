/*
 * Copyright 2016-2017 MessageML - Symphony LLC
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

package org.symphonyoss.symphony.messageml.util;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;

public class CountedOutputStreamTest {

  private ByteArrayOutputStream baos;
  private CountedOutputStream out;

  @Before
  public void setUp() {
    baos = new ByteArrayOutputStream();
    out = new CountedOutputStream(baos);
  }

  @After
  public void tearDown() throws Exception {
    out.flush();
    out.close();
  }

  @Test
  public void testWriteChar() throws Exception {
    out.write('a');
    out.write('b');
    out.write('c');
    assertEquals("Output", "abc", baos.toString());
    assertEquals("Offset", 3, out.getOffset());
  }

  @Test
  public void testWriteByteArray() throws Exception {
    out.write("abc".getBytes());
    assertEquals("Output", "abc", baos.toString());
    assertEquals("Offset", 3, out.getOffset());
  }

  @Test
  public void testWriteByteArraySlice() throws Exception {
    out.write("abcd".getBytes(), 1, 2);
    assertEquals("Output", "bc", baos.toString());
    assertEquals("Offset", 2, out.getOffset());
  }

  @Test
  public void testGetOffset() throws Exception {
    assertEquals("Offset", 0, out.getOffset());
  }

  @Test
  public void testnUncounted() throws Exception {
    out.write('a');
    out.beginUncounted();
    out.write('b');
    out.write('c');
    out.endUncounted();
    out.write('d');
    assertEquals("Output", "abcd", baos.toString());
    assertEquals("Offset", 2, out.getOffset());
  }

}
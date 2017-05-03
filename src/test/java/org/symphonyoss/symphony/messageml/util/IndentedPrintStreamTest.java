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

import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.Collections;


public class IndentedPrintStreamTest {

  private ByteArrayOutputStream outputStream;
  private IndentedPrintStream printStream;

  @Before
  public void setUp() {
    outputStream = new ByteArrayOutputStream();
    printStream = new IndentedPrintStream(outputStream);
  }

  @Test
  public void testSetPrintOffsets() throws Exception {
    printStream.setPrintOffsets(true);
    assertEquals("Print offsets", true, printStream.getPrintOffsets());

    printStream.println("Hello");
    printStream.println("world!");
    assertEquals("Output", "         0 Hello\n         6 world!\n", outputStream.toString());
    assertEquals("Offset", 13, printStream.getOffset());
  }

  @Test
  public void testUnsetPrintOffsets() throws Exception {
    printStream.setPrintOffsets(false);
    assertEquals("Print offsets", false, printStream.getPrintOffsets());

    printStream.print("Hello world!");
    assertEquals("Output", "Hello world!", outputStream.toString());
  }

  @Test
  public void testSetLinePrefix() throws Exception {
    printStream.setLinePrefix("| ");
    assertEquals("Line prefix", "| ", printStream.getLinePrefix());

    printStream.print("Hello world!");
    assertEquals("Output", "| Hello world!", outputStream.toString());
  }

  @Test
  public void testAlign() throws Exception {
    printStream.align("This", "is", "an", "aligned", "block");
    printStream.printAlignedBlock();
    assertEquals("Output", "This    is      an      aligned block\n", outputStream.toString());
  }


  @Test
  public void testBlock() throws Exception {
    printStream.openBlock();
    printStream.closeBlock();
    assertEquals("Output", "{\n}\n", outputStream.toString());
  }

  @Test
  public void testBlockWithString() throws Exception {
    printStream.openBlock("prefix ");
    printStream.continueBlock(" infix ");
    printStream.closeBlock(" postfix");
    assertEquals("Output", "prefix \n{\n}\n infix \n{\n} postfix\n", outputStream.toString());
  }

  @Test
  public void testPrintBooleanWithIndent() throws Exception {
    printStream.print(4, true);
    assertEquals("Output", "\t\t\t\ttrue", outputStream.toString());
  }

  @Test
  public void testPrintCharWithIndent() throws Exception {
    printStream.print(4, 'x');
    assertEquals("Output", "\t\t\t\tx", outputStream.toString());
  }

  @Test
  public void testPrintCharArrayWithIndent() throws Exception {
    printStream.print(4, new char[] {'x', 'y', 'z'});
    assertEquals("Output", "\t\t\t\txyz", outputStream.toString());
  }

  @Test
  public void testPrintDoubleWithIndent() throws Exception {
    printStream.print(4, 1.0);
    assertEquals("Output", "\t\t\t\t1.0", outputStream.toString());
  }

  @Test
  public void testPrintFloatWithIndent() throws Exception {
    printStream.print(4, 1.0F);
    assertEquals("Output", "\t\t\t\t1.0", outputStream.toString());
  }

  @Test
  public void testPrintIntWithIndent() throws Exception {
    printStream.print(4, 1);
    assertEquals("Output", "\t\t\t\t1", outputStream.toString());
  }

  @Test
  public void testPrintObjectWithIndent() throws Exception {
    Object obj = Collections.emptyList();
    printStream.print(4, obj);
    assertEquals("Output", "\t\t\t\t" + obj.toString(), outputStream.toString());
  }

  @Test
  public void testPrintStringWithIndex() throws Exception {
    printStream.print(4, "XYZ");
    assertEquals("Output", "\t\t\t\tXYZ", outputStream.toString());
  }

  @Test
  public void testPrintLongWithIndent() throws Exception {
    printStream.print(4, 1L);
    assertEquals("Output", "\t\t\t\t1", outputStream.toString());
  }

  @Test
  public void testPrintln() throws Exception {
    printStream.println();
    assertEquals("Output", "\n", outputStream.toString());
  }

  @Test
  public void testForceNewLine() throws Exception {
    printStream.forceNewLine();
    assertEquals("Output", "", outputStream.toString());
  }

  @Test
  public void testPrintlnBooleanWithIndent() throws Exception {
    printStream.println(4, true);
    assertEquals("Output", "\t\t\t\ttrue\n", outputStream.toString());
  }

  @Test
  public void testPrintlnCharWithIndent() throws Exception {
    printStream.println(4, 'x');
    assertEquals("Output", "\t\t\t\tx\n", outputStream.toString());
  }

  @Test
  public void testPrintlnCharArrayWithIndent() throws Exception {
    printStream.println(4, new char[] {'x', 'y', 'z'});
    assertEquals("Output", "\t\t\t\txyz\n", outputStream.toString());
  }

  @Test
  public void testPrintlnDoubleWithIndent() throws Exception {
    printStream.println(4, 1.0);
    assertEquals("Output", "\t\t\t\t1.0\n", outputStream.toString());
  }

  @Test
  public void testPrintlnFloatWithIndent() throws Exception {
    printStream.println(4, 1.0F);
    assertEquals("Output", "\t\t\t\t1.0\n", outputStream.toString());
  }

  @Test
  public void testPrintlnIntWithIndent() throws Exception {
    printStream.println(4, 1);
    assertEquals("Output", "\t\t\t\t1\n", outputStream.toString());
  }

  @Test
  public void testPrintlnObjectWithIndent() throws Exception {
    Object obj = Collections.emptyList();
    printStream.println(4, obj);
    assertEquals("Output", "\t\t\t\t" + obj.toString() + "\n", outputStream.toString());
  }

  @Test
  public void testPrintlnStringWithIndent() throws Exception {
    printStream.println(4, "XYZ");
    assertEquals("Output", "\t\t\t\tXYZ\n", outputStream.toString());
  }

  @Test
  public void testPrintlnLongWithIndent() throws Exception {
    printStream.println(4, 1L);
    assertEquals("Output", "\t\t\t\t1\n", outputStream.toString());
  }

  @Test
  public void testPrintBoolean() throws Exception {
    printStream.print(true);
    assertEquals("Output", "true", outputStream.toString());
  }

  @Test
  public void testPrintChar() throws Exception {
    printStream.print('x');
    assertEquals("Output", "x", outputStream.toString());
  }

  @Test
  public void testPrintCharArray() throws Exception {
    printStream.print(new char[] {'x', 'y', 'z'});
    assertEquals("Output", "xyz", outputStream.toString());
  }

  @Test
  public void testPrintDouble() throws Exception {
    printStream.print(1.0);
    assertEquals("Output", "1.0", outputStream.toString());
  }

  @Test
  public void testPrintFloat() throws Exception {
    printStream.print(1.0F);
    assertEquals("Output", "1.0", outputStream.toString());
  }

  @Test
  public void testPrintInt() throws Exception {
    printStream.print(1);
    assertEquals("Output", "1", outputStream.toString());
  }

  @Test
  public void testPrintObject() throws Exception {
    Object obj = Collections.emptyList();
    printStream.print(obj);
    assertEquals("Output", obj.toString(), outputStream.toString());
  }

  @Test
  public void testPrintString() throws Exception {
    printStream.print("XYZ");
    assertEquals("Output", "XYZ", outputStream.toString());
  }

  @Test
  public void testPrintLong() throws Exception {
    printStream.print(1L);
    assertEquals("Output", "1", outputStream.toString());
  }

  @Test
  public void testPrintlnBoolean() throws Exception {
    printStream.println(true);
    assertEquals("Output", "true\n", outputStream.toString());
  }

  @Test
  public void testPrintlnChar() throws Exception {
    printStream.println('x');
    assertEquals("Output", "x\n", outputStream.toString());
  }

  @Test
  public void testPrintlnCharArray() throws Exception {
    printStream.println(new char[] {'x', 'y', 'z'});
    assertEquals("Output", "xyz\n", outputStream.toString());
  }

  @Test
  public void testPrintlnDouble() throws Exception {
    printStream.println(1.0);
    assertEquals("Output", "1.0\n", outputStream.toString());
  }

  @Test
  public void testPrintlnFloat() throws Exception {
    printStream.println(1.0F);
    assertEquals("Output", "1.0\n", outputStream.toString());
  }

  @Test
  public void testPrintlnInt() throws Exception {
    printStream.println(1);
    assertEquals("Output", "1\n", outputStream.toString());
  }

  @Test
  public void testPrintlnObject() throws Exception {
    Object obj = Collections.emptyList();
    printStream.println(obj);
    assertEquals("Output", obj.toString() + "\n", outputStream.toString());
  }

  @Test
  public void testPrintlnString() throws Exception {
    printStream.println("XYZ");
    assertEquals("Output", "XYZ\n", outputStream.toString());
  }

  @Test
  public void testPrintlnLong() throws Exception {
    printStream.println(1L);
    assertEquals("Output", "1\n", outputStream.toString());
  }

  @Test
  public void testPrintlines() throws Exception {
    printStream.printlines("X", "Y", "Z");
    assertEquals("Output", "X\nY\nZ\n", outputStream.toString());
  }

  @Test
  public void testPrintlnStringCollection() throws Exception {
    printStream.println(Arrays.asList("X", "Y", "Z"));
    assertEquals("Output", "X\nY\nZ\n", outputStream.toString());
  }

  @Test
  public void testPrintWithFormat() throws Exception {
    printStream.print("%s\t%s", "A", "B");
    assertEquals("Output", "A\tB", outputStream.toString());
  }

  @Test
  public void testPrintlnWithFormat() throws Exception {
    printStream.println("%s\t%s", "A", "B");
    assertEquals("Output", "A\tB\n", outputStream.toString());
  }

  @Test
  public void testPrintWithFormatAndIndent() throws Exception {
    printStream.print(4,"%s | %s", "A", "B");
    assertEquals("Output", "\t\t\t\tA | B", outputStream.toString());
  }

  @Test
  public void testPrintlnWithFormatAndIndent() throws Exception {
    printStream.println(4,"%s | %s", "A", "B");
    assertEquals("Output", "\t\t\t\tA | B\n", outputStream.toString());
  }

  @Test
  public void testSetGetONlCr() throws Exception {
    printStream.setoNlCr(true);
    assertEquals("ONlCr", true, printStream.isoNlCr());

    printStream.setoNlCr(false);
    assertEquals("ONlCr", false, printStream.isoNlCr());
  }

  @Test
  public void testSetGetNoIndent() throws Exception {
    printStream.setNoIndent(true);
    assertEquals("ONlCr", true, printStream.isNoIndent());

    printStream.setNoIndent(false);
    assertEquals("ONlCr", false, printStream.isNoIndent());
  }

  @Test
  public void testSetGetNoNl() throws Exception {
    printStream.setNoNl(true);
    assertEquals("ONlCr", true, printStream.isNoNl());

    printStream.setNoNl(false);
    assertEquals("ONlCr", false, printStream.isNoNl());
  }

}
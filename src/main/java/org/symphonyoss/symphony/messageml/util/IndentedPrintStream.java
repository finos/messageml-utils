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

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Collection;

/**
 * A PrintStream with added indenting functionality.
 * @author Bruce Skingle
 */
public class IndentedPrintStream extends PrintStream {
  private boolean startOfLine = true;
  private int indent = 0;
  private AlignedBlock alignedBlock = null;
  private boolean printOffsets = false;
  private boolean oNlCr = false;
  private boolean noIndent = false;
  private boolean noNl = false;
  private boolean removeNl = true;
  private String linePrefix;
  private int linePrefixIndent = 9999;


  public IndentedPrintStream(OutputStream outputStream) {
    super(outputStream instanceof CountedOutputStream ? outputStream : new CountedOutputStream(outputStream));
  }

  public void setPrintOffsets(boolean printOffsets) {
    this.printOffsets = printOffsets;
  }

  public boolean getPrintOffsets() {
    return this.printOffsets;
  }

  public void setLinePrefix(String linePrefix) {
    this.linePrefix = linePrefix;
    linePrefixIndent = linePrefix == null ? 9999 : indent;
  }

  public String getLinePrefix() {
    return this.linePrefix;
  }

  /**
   * Takes an array of strings to be aligned in separate columns, and adds them to the alignedblock
   */
  public void align(Object... s) {
    if (alignedBlock == null) {
      alignedBlock = new AlignedBlock(this);
    }
    alignedBlock.align(s);
  }

  /**
   * Prints the alignedblock, with all strings aligned in columns dependent on the order in which they were declared in Align
   * @param separator text added to the end of each line except the last line
   * @param terminator text added to the end of the last line of the block
   */
  private void printAlignedBlock(String separator, String terminator) {
    if (alignedBlock != null) {
      alignedBlock.print(separator, terminator);
      alignedBlock = null;
    }
  }

  /**
   * Prints the alignedblock without any separators or terminators
   */
  public void printAlignedBlock() {
    printAlignedBlock(null, null);
  }

  /**
   * prints an open curly bracket and indents the following line
   */
  public void openBlock() {
    println("{");
    indent();
  }

  /**
   * Outdents the line and prints a close curly bracket on the following line
   */
  public void closeBlock() {
    outdent();
    println("}");
  }

  /**
   * Prints a string, an open curly bracket on its own line and indents the following line
   * @param s preceding string
   */
  public void openBlock(String s) {
    println(s);
    println("{");
    indent();
  }

  /**
   * Outdents the current line and prints a close curly bracket on the next line, followed by a string
   * @param s succeeding string
   */
  public void closeBlock(String s) {
    outdent();
    println("}" + s);
  }

  /**
   * Ends one block (outdent followed by a close curly bracket on the following line) prints a string
   * on its own line and begins a new block (open curly bracket followed by an indent on the following
   * line
   * @param s intermediate string
   */
  public void continueBlock(String s) {
    outdent();
    println("}");
    println(s);
    println("{");
    indent();
  }

  /**
   * Increases the indent on the current line
   */
  public void indent() {
    indent++;
  }

  /**
   * Decreases the indent on the current line
   */
  public void outdent() {
    indent--;
  }


  private void doIndent(int tempIndent) {
    if (printOffsets) {
      ((CountedOutputStream) super.out).beginUncounted();
      super.print(String.format("%10d ", getOffset()));
      ((CountedOutputStream) super.out).endUncounted();
    }

    if (!noIndent) {
      int i;

      tempIndent += indent;

      for (i = 0; i < tempIndent && i < linePrefixIndent; i++) {
        super.print('\t');
      }

      if (linePrefix != null) { super.print(linePrefix); }

      while (i++ < tempIndent) {
        super.print('\t');
      }
    }
    startOfLine = false;
  }


  /**
   * Indent the current line and print a boolean on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param b boolean to be printed
   */
  public void print(int tempIndent, boolean b) {
    print(tempIndent, "" + b);
  }

  /**
   * Indent the current line and print a character on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param c character to be printed
   */
  public void print(int tempIndent, char c) {
    if (startOfLine) {
      doIndent(tempIndent);
    }
    super.print(c);
  }

  /**
   * Indent the current line and print a character array on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param s character array to be printed
   */
  public void print(int tempIndent, char[] s) {
    if (startOfLine) {
      doIndent(tempIndent);
    }
    super.print(s);
  }

  /**
   * Indent the current line and print a double on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param d double to be printed
   */
  public void print(int tempIndent, double d) {
    print(tempIndent, "" + d);
  }

  /**
   * Indent the current line and print a floating point number on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param f floating point number to be printed
   */
  public void print(int tempIndent, float f) {
    print(tempIndent, "" + f);
  }

  /**
   * Indent the current line and print an integer on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param i integer to be printed
   */
  public void print(int tempIndent, int i) {
    print(tempIndent, "" + i);
  }

  /**
   * Indent the current line and print an object on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param obj object to be printed
   */
  public void print(int tempIndent, Object obj) {
    print(tempIndent, "" + obj);
  }

  /**
   * Indent the current line and print a string on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param s string to be printed
   */
  public void print(int tempIndent, String s) {
    if (startOfLine) {
      doIndent(tempIndent);
    }
    super.print(s);
  }

  /**
   * Indent the current line and print a long integer on the same line
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param l long integer to be printed
   */
  public void print(int tempIndent, long l) {
    print(tempIndent, "" + l);
  }

  /**
   * Prints a linebreak
   */
  @Override
  public void println() {
    super.println();
    startOfLine = true;
  }

  /**
   * Print a linebreak unless we are at the start of a line.
   */
  public void forceNewLine() {
    if (!startOfLine) { println(); }
  }

  /**
   * Indent the current line and print a boolean on the same line,
   * then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x boolean to be printed
   */
  public void println(int tempIndent, boolean x) {
    println(tempIndent, "" + x);
  }

  /**
   * Indent the current line and print a character on the same line,
   * then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x character to be printed
   */
  public void println(int tempIndent, char x) {
    if (startOfLine) {
      doIndent(tempIndent);
    }
    super.println(x);
    startOfLine = true;
  }

  /**
   * Indent the current line and print a character array on the same line,
   * then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x character array to be printed
   */
  public void println(int tempIndent, char[] x) {
    if (startOfLine) {
      doIndent(tempIndent);
    }
    super.println(x);
    startOfLine = true;
  }

  /**
   * Indent the current line and print a double on the same line,
   * then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x double to be printed
   */
  public void println(int tempIndent, double x) {
    println(tempIndent, "" + x);
  }

  /**
   * Indent the current line and print a floating point number
   * on the same line, then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x floating point number to be printed
   */
  public void println(int tempIndent, float x) {
    println(tempIndent, "" + x);
  }

  /**
   * Indent the current line and print an integer on the same line,
   * then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x integer to be printed
   */
  public void println(int tempIndent, int x) {
    println(tempIndent, "" + x);
  }

  /**
   * Indent the current line and print an object on the same line,
   * then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x object to be printed
   */
  public void println(int tempIndent, Object x) {
    println(tempIndent, "" + x);
  }

  /**
   * Indent the current line and print a string on the same line,
   * then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x string to be printed
   */
  public void println(int tempIndent, String x) {
    if (startOfLine) {
      doIndent(tempIndent);
    }

    if (noNl) {
      super.print(x);
    } else {
      super.println(x);
      if (oNlCr) { super.write('\r'); }
    }
    startOfLine = true;
  }

  /**
   * Indent the current line and print a long integer on the same line,
   * then print a line break
   * @param tempIndent magnitude of the indent (number of tab spaces)
   * @param x long integer to be printed
   */
  public void println(int tempIndent, long x) {
    println(tempIndent, "" + x);
  }

  /**
   * print a boolean with no indent
   * @param b boolean to print
   */
  @Override
  public void print(boolean b) {
    print(0, b);
  }

  /**
   * print a character with no indent
   * @param c character to print
   */
  @Override
  public void print(char c) {
    print(0, c);
  }

  /**
   * print a character array with no indent
   * @param s character array to print
   */
  @Override
  public void print(char[] s) {
    print(0, s);
  }

  /**
   * print a double with no indent
   * @param d double to print
   */
  @Override
  public void print(double d) {
    print(0, d);
  }

  /**
   * print a floating point number with no indent
   * @param f floating point number to print
   */
  @Override
  public void print(float f) {
    print(0, f);
  }

  /**
   * print an integer with no indent
   * @param i integer to print
   */
  @Override
  public void print(int i) {
    print(0, i);
  }

  /**
   * print an object with no indent
   * @param obj object to print
   */
  @Override
  public void print(Object obj) {
    print(0, obj);
  }

  /**
   * print a string with no indent
   * @param s string to print
   */
  @Override
  public void print(String s) {
    print(0, s);
  }

  /**
   * print a long integer with no indent
   * @param l long integer to print
   */
  @Override
  public void print(long l) {
    print(0, l);
  }

  /**
   * print a boolean with no indent with a line break
   * @param x boolean to print
   */
  @Override
  public void println(boolean x) {
    println(0, x);
  }

  /**
   * print a character with no indent with a line break
   * @param x character to print
   */
  @Override
  public void println(char x) {
    println(0, x);
  }

  /**
   * print a character array with no indent with a line break
   * @param x character array to print
   */
  @Override
  public void println(char[] x) {
    println(0, x);
  }

  /**
   * print a double with no indent with a line break
   * @param x double to print
   */
  @Override
  public void println(double x) {
    println(0, x);
  }

  /**
   * print a floating point number with no indent with a line break
   * @param x floating point number to print
   */
  @Override
  public void println(float x) {
    println(0, x);
  }

  /**
   * print an integer with no indent with a line break
   * @param x integer to print
   */
  @Override
  public void println(int x) {
    println(0, x);
  }

  /**
   * print an object with no indent with a line break
   * @param x object to print
   */
  @Override
  public void println(Object x) {
    println(0, x);
  }

  /**
   * print a string with no indent with a line break
   * @param x string to print
   */
  @Override
  public void println(String x) {
    println(0, x);
  }

  /**
   * Print multiple strings, each with a line break.
   * @param strings multiple strings to print.
   */
  public void printlines(String... strings) {
    for (String s : strings) { println(s); }
  }

  /**
   * print a long integer with no indent with a line break
   * @param x long integer to print
   */
  @Override
  public void println(long x) {
    println(0, x);
  }

  /**
   * Prints all strings in a collection on individual lines
   * @param str collection of strings
   */
  public void println(Collection<String> str) {
    for (String s : str) {
      println(0, s);
    }
  }

  /**
   * Convenience method for calling string.format and printing
   */
  public void print(String pattern, Object... arguments) {
    print(String.format(pattern, arguments));
  }

  /**
   * Convenience method for calling string.format and printing
   * with line breaks
   */
  public void println(String pattern, Object... arguments) {
    println(String.format(pattern, arguments));
  }

  /**
   * Calls string.format, indents and prints
   */
  public void print(int indent, String pattern, Object... arguments) {
    print(indent, String.format(pattern, arguments));
  }

  /**
   * Calls string.format, indents and prints with line break
   */
  public void println(int indent, String pattern, Object... arguments) {
    println(indent, String.format(pattern, arguments));
  }

  public long getOffset() {
    return ((CountedOutputStream) super.out).getOffset();
  }

  public boolean isoNlCr() {
    return oNlCr;
  }

  public void setoNlCr(boolean oNlCr) {
    this.oNlCr = oNlCr;
  }

  public boolean isNoIndent() {
    return noIndent;
  }

  public void setNoIndent(boolean noIndent) {
    this.noIndent = noIndent;
  }

  public boolean isNoNl() {
    return noNl;
  }

  public void setNoNl(boolean noNl) {
    this.noNl = noNl;
  }

  public boolean isRemoveNl() {
    return removeNl;
  }

  public void setRemoveNl(boolean removeNl) {
    this.removeNl = removeNl;
  }
}

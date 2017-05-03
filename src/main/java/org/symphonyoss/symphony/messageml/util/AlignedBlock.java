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

import java.util.ArrayList;

/**
 * A block of tabulated text.
 * Rows can be added with the align() method, each string representing the text for an aligned column.
 * The print method outputs the block, optionally outputting a separator.
 * @author Bruce Skingle
 */
public class AlignedBlock {
  private static final int TAB_SIZE = 8;

  private final IndentedPrintStream out;
  private final ArrayList<Integer> maxColumnLength = new ArrayList<>();
  private final ArrayList<String[]> rows = new ArrayList<>();

  /**
   * Constructor.
   * @param out An IndentedPrintStream to which the block will be output.
   */
  public AlignedBlock(IndentedPrintStream out) {
    this.out = out;
  }

  /**
   * Add a row to the block, each String represents a piece of text which should be aligned.
   * @param o Variable number of Strings to align.
   */
  public void align(Object... o) {
    int i = 0;
    String s[] = new String[o.length];

    for (int ii = 0; ii < o.length; ii++) { if (o[ii] != null) { s[ii] = o[ii].toString(); } }

    while (i < s.length && i < maxColumnLength.size()) {
      int l = (s[i] == null ? 0 : s[i].length());

      if (maxColumnLength.get(i) < l) {
        maxColumnLength.set(i, l);
      }

      i++;
    }

    while (i < s.length) {
      maxColumnLength.add((s[i] == null ? 0 : s[i].length()));
      i++;
    }

    rows.add(s);
  }

  /**
   * Outputs the block, with the given separator appended to each line except the last, and the given
   * terminator appended to the last line.
   * @param separator text added to each line except the last/
   * @param terminator text added to the last line.
   */
  public void print(String separator, String terminator) {
    for (int i = 0; i < maxColumnLength.size(); i++) {
      maxColumnLength.set(i, (((maxColumnLength.get(i) / TAB_SIZE) + 1) * TAB_SIZE));
    }

    for (int r = 0; r < rows.size(); r++) {
      String[] s = rows.get(r);

      for (int i = 0; i < s.length; i++) {
        out.print(s[i]);

        if (i < s.length - 1) {
          for (int l = (s[i] == null ? 0 : s[i].length()); l < maxColumnLength.get(i); l++) {
            out.print(' ');
          }
        }
      }
      if (separator != null && r < rows.size() - 1) { out.println(separator); } else if (terminator != null
          && r == rows.size() - 1) { out.println(terminator); } else { out.println(); }
    }
  }

  /**
   * Output the block without additional separators.
   */
  public void print() {
    print(null, null);
  }
}


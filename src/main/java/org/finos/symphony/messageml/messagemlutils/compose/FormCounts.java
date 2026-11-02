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

package org.finos.symphony.messageml.messagemlutils.compose;

/**
 * How many of each Symphony form one render actually produced.
 *
 * <p>This is what makes contract adherence observable in production rather than only on a benchmark
 * corpus: a corpus says what a model does on its fixtures, these counters say what it does on real
 * traffic. Counting the <em>produced</em> forms rather than the recognised source forms is deliberate - * a form that degraded to text is not a form the user saw.
 *
 * <p>Returned as an immutable value so the converter stays a shared singleton with no per-request state.
 *
 * @param nativeRendered how many native {@code <messageML>} blocks were inlined unchanged
 * @param nativeRepaired how many needed the deterministic repair pass before they could be inlined
 * @param nativeDegraded how many were replaced by the degraded card, on the final render only
 *                       (a streaming render never reaches the metrics)
 */
public record FormCounts(
    int mention, int hash, int cash, int card, int expandable,
    int nativeRendered, int nativeRepaired, int nativeDegraded) {

  public static final FormCounts NONE = new FormCounts(0, 0, 0, 0, 0, 0, 0, 0);

  /** {@code true} when nothing beyond standard Markdown was produced - the common case. */
  public boolean isEmpty() {
    return mention == 0 && hash == 0 && cash == 0 && card == 0 && expandable == 0
        && nativeRendered == 0 && nativeRepaired == 0 && nativeDegraded == 0;
  }

  /** Mutable accumulator used during a single AST walk. Not thread-safe and never shared. */
  static final class Tally {
    private int mention;
    private int hash;
    private int cash;
    private int card;
    private int expandable;
    private int nativeRendered;
    private int nativeRepaired;
    private int nativeDegraded;

    void mention() {
      mention++;
    }

    void hash() {
      hash++;
    }

    void cash() {
      cash++;
    }

    void card() {
      card++;
    }

    void expandable() {
      expandable++;
    }

    void nativeRendered() {
      nativeRendered++;
    }

    void nativeRepaired() {
      nativeRepaired++;
    }

    void nativeDegraded() {
      nativeDegraded++;
    }

    FormCounts snapshot() {
      return new FormCounts(mention, hash, cash, card, expandable, nativeRendered, nativeRepaired, nativeDegraded);
    }
  }
}

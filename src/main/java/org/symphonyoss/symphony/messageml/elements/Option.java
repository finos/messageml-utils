package org.symphonyoss.symphony.messageml.elements;

/**
 * Class representing a Symphony Elements option
 *
 * @author lumoura
 * @since 03/22/19
 */
public class Option extends Element {

  public static final String MESSAGEML_TAG = "option";

  public Option(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  /* TODO:  The code for this class is supposed to be implemented in task APP-2057.
     This class was added here so the code for the Select class (APP-2056) could be complete.
  */
}
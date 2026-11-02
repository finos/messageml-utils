package org.finos.symphony.messageml.messagemlutils.compose;

import org.finos.symphony.messageml.messagemlutils.elements.Button;
import org.finos.symphony.messageml.messagemlutils.elements.Dialog;
import org.finos.symphony.messageml.messagemlutils.elements.Form;
import org.finos.symphony.messageml.messagemlutils.elements.FormElement;
import org.finos.symphony.messageml.messagemlutils.elements.UIAction;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.File;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.TreeSet;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Hand-written cases for {@link MessageMLSanitizer}. The Elements-derivation test is the
 * self-correcting half: it reads the real tag names out
 * of the pinned {@code messageml-utils} jar via reflection so {@link MessageMLSanitizer#SYMPHONY_ELEMENTS}
 * cannot silently drift from the library it is supposed to police.
 */
class MessageMLSanitizerTest {

  private final MessageMLSanitizer sanitizer = new MessageMLSanitizer();

  @Test
  void dollarInterpolationInText_isEscaped() {
    assertEquals("<p>Total &#36;{amount}</p>", sanitizer.sanitize("<p>Total ${amount}</p>"));
  }

  @Test
  void hashInterpolationInText_isEscaped() {
    assertEquals("<p>Total &#35;{amount}</p>", sanitizer.sanitize("<p>Total #{amount}</p>"));
  }

  @Test
  void dollarInterpolationInAttributeValue_isEscaped() {
    assertEquals("<div title=\"&#36;{amount}\">x</div>",
        sanitizer.sanitize("<div title=\"${amount}\">x</div>"));
  }

  @Test
  void hashInterpolationInAttributeValue_isEscaped() {
    assertEquals("<div title=\"&#35;{amount}\">x</div>",
        sanitizer.sanitize("<div title=\"#{amount}\">x</div>"));
  }

  @Test
  void freemarkerDirective_isRejected() {
    assertNull(sanitizer.sanitize("<p>a<#assign x=1>b</p>"));
  }

  @Test
  void freemarkerClosingDirective_isRejected() {
    assertNull(sanitizer.sanitize("<p>a</#if>b</p>"));
  }

  @Test
  void freemarkerMacroCall_isRejected() {
    assertNull(sanitizer.sanitize("<p>a<@macro/>b</p>"));
  }

  @Test
  void freemarkerClosingMacro_isRejected() {
    assertNull(sanitizer.sanitize("<p>a</@macro>b</p>"));
  }

  // The scanner reads "<#if a < b>" as plain text (a '<' runs into the tag before its '>'), so the
  // rejection has to come from the raw-string check, not from an OpenTag/CloseTag name.
  @Test
  void freemarkerDirectiveContainingALessThan_isRejected() {
    assertNull(sanitizer.sanitize("<p><#if a < b>x</#if></p>"));
    assertNull(sanitizer.sanitize("<p><@macro a<b/></p>"));
  }

  @Test
  void freemarkerDirectiveInAnAttributeValue_isRejected() {
    assertNull(sanitizer.sanitize("<div style=\"<#assign x=1>\">x</div>"));
  }

  @Test
  void freemarkerDirectiveInACommentOrCdata_isRejected() {
    assertNull(sanitizer.sanitize("<p>a<!-- <#assign x=1> -->b</p>"));
    assertNull(sanitizer.sanitize("<p><![CDATA[<@macro/>]]></p>"));
  }

  @Test
  void interpolationOutsideAnyTextOrQuotedValue_isEscapedAnyway() {
    assertEquals("<p &#36;{7*7}>a<!-- &#35;{x} --><![CDATA[&#36;{y}]]></p>",
        sanitizer.sanitize("<p ${7*7}>a<!-- #{x} --><![CDATA[${y}]]></p>"));
  }

  @Test
  void interpolationInAnUnfinishedTail_isEscaped() {
    assertEquals("<p>x</p><div title=\"&#36;{secret", sanitizer.sanitize("<p>x</p><div title=\"${secret"));
  }

  /**
   * messageml-utils only runs FreeMarker on a message containing one of these four, so none may ever
   * come out of the sanitizer.
   */
  @ParameterizedTest
  @MethodSource("hostileFragments")
  void sanitizedFragment_neverContainsAnythingThatStartsTheTemplateEngine(String fragment) {
    String sanitized = sanitizer.sanitize(fragment);
    if (sanitized != null) {
      for (String trigger : new String[] {"<#", "<@", "${", "#{"}) {
        assertFalse(sanitized.contains(trigger), () -> trigger + " survived in " + sanitized);
      }
    }
  }

  static Stream<String> hostileFragments() {
    return Stream.of(
        "<p>${x}</p>", "<p>#{x}</p>", "<p>$${x}}</p>", "<p title='${x}'>a</p>", "<p ${x}>a</p>",
        "<p>a<!-- ${x} --></p>", "<p><![CDATA[#{x}]]></p>", "<p>${\"freemarker.template.utility.Execute\"?new()(\"id\")}</p>",
        "<p>a <#if x>b</#if></p>", "<p>a <@m/></p>", "<p title=\"<@m/>\">a</p>", "<p>tail ${unfinished");
  }

  @Test
  void symphonyElementWrittenInUpperCase_isRejected() {
    assertNull(sanitizer.sanitize("<FORM id=\"f\"><Button name=\"b\">ok</Button></FORM>"));
  }

  @Test
  void entityClassWithSingleQuotesOrAnotherCase_isRejected() {
    assertNull(sanitizer.sanitize("<div class='entity'>An object</div>"));
    assertNull(sanitizer.sanitize("<div CLASS=\"Entity\">An object</div>"));
  }

  @Test
  void ordinaryClassContainingTheWordEntity_isNotRejected() {
    assertEquals("<span class=\"tempo-entity-label\">x</span>",
        sanitizer.sanitize("<span class=\"tempo-entity-label\">x</span>"));
  }

  @ParameterizedTest
  @MethodSource("symphonyElementTags")
  void everySymphonyElementTag_isRejected(String tag) {
    assertNull(sanitizer.sanitize("<" + tag + ">x</" + tag + ">"), "expected <" + tag + "> to be rejected");
  }

  static Stream<String> symphonyElementTags() {
    return MessageMLSanitizer.SYMPHONY_ELEMENTS.stream();
  }

  @Test
  void entityClassAttribute_isRejected() {
    assertNull(sanitizer.sanitize("<div class=\"entity\" data-entity-id=\"x\">An object</div>"));
  }

  @Test
  void dataEntityIdAttributeAlone_isRejected() {
    assertNull(sanitizer.sanitize("<span data-entity-id=\"x\">An object</span>"));
  }

  @Test
  void ordinaryFragment_isReturnedWithOnlyInterpolationsEscaped() {
    String fragment = "<h1>Title</h1><p>Body with ${x} and plain text.</p>";
    String expected = "<h1>Title</h1><p>Body with &#36;{x} and plain text.</p>";

    assertEquals(expected, sanitizer.sanitize(fragment));
  }

  /**
   * The list was first hand-copied from the messageml-utils element classes for the pinned version and
   * is asserted by a test, so an upgrade that adds an element fails loudly. Deriving it turned up two
   * corrections against the hand-written list itself (not just future upgrades): {@code country-code}
   * is only ever an attribute name ({@code TagAttributes.ATTR_COUNTRYCODE}) in messageml-utils 1.5.0,
   * never its own element and
   * {@code dropdown-menu} is implemented as a plain {@code <select>} - neither exists as a
   * {@code MESSAGEML_TAG}. Conversely {@code date-picker} (a real, separate element from
   * {@code date-selector}) and {@code richtextarea} (a {@code TextArea} subclass, easy to miss by
   * eye since it does not show up in the symphony-elements-1 doc page the corpus was built from)
   * were both missing from the hand-written list. See
   * {@link MessageMLSanitizer#SYMPHONY_ELEMENTS}'s javadoc for the full explanation.
   *
   * <p>Ground truth: every non-abstract subclass of {@code FormElement} in
   * {@code org.finos.symphony.messageml.messagemlutils.elements} (the library's own base class for a
   * control that must live inside a {@code <form>}) plus the four wrapper elements that create or
   * trigger one directly and are not themselves {@code FormElement}s: {@code Form}, {@code Button},
   * {@code UIAction} and {@code Dialog}. Each tag name is read off the class's own
   * {@code MESSAGEML_TAG} constant via reflection, not retyped.
   */
  @Test
  void symphonyElementsAllowlist_isDerivedFromTheLibraryElements() throws Exception {
    TreeSet<String> derived = new TreeSet<>();
    derived.add(tagOf(Form.class));
    derived.add(tagOf(Button.class));
    derived.add(tagOf(UIAction.class));
    derived.add(tagOf(Dialog.class));
    derived.addAll(formElementSubclassTags());

    assertEquals(derived, MessageMLSanitizer.SYMPHONY_ELEMENTS);
  }

  private static String tagOf(Class<?> clazz) throws ReflectiveOperationException {
    Object value = clazz.getField("MESSAGEML_TAG").get(null);
    return (String) value;
  }

  private static TreeSet<String> formElementSubclassTags() throws Exception {
    TreeSet<String> tags = new TreeSet<>();
    for (String className : elementClassNames()) {
      Class<?> candidate = Class.forName(className);
      if (candidate.isInterface() || Modifier.isAbstract(candidate.getModifiers())) {
        continue;
      }
      if (candidate == FormElement.class || !FormElement.class.isAssignableFrom(candidate)) {
        continue;
      }
      try {
        tags.add(tagOf(candidate));
      } catch (NoSuchFieldException ignored) {
        // No own tag constant to read; its parent's is already covered by this same scan.
      }
    }
    return tags;
  }

  /**
   * Names of the top-level classes in the elements package and its sub-packages, read from the code
   * source of {@link FormElement}: a directory such as {@code target/classes} when the test runs in
   * this build, or a jar when the library is on the classpath as a dependency.
   */
  private static List<String> elementClassNames() throws Exception {
    URL location = FormElement.class.getProtectionDomain().getCodeSource().getLocation();
    assertNotNull(location, "expected the elements package to have a code source");
    String prefix = FormElement.class.getPackageName().replace('.', '/') + "/";
    File codeSource = new File(location.toURI());

    List<String> entryNames = new ArrayList<>();
    if (codeSource.isDirectory()) {
      Path root = codeSource.toPath();
      try (Stream<Path> files = Files.walk(root.resolve(prefix))) {
        files.filter(Files::isRegularFile)
            .map(file -> root.relativize(file).toString().replace(File.separatorChar, '/'))
            .forEach(entryNames::add);
      }
    } else {
      try (JarFile jar = new JarFile(codeSource)) {
        Enumeration<JarEntry> entries = jar.entries();
        while (entries.hasMoreElements()) {
          entryNames.add(entries.nextElement().getName());
        }
      }
    }

    List<String> classNames = new ArrayList<>();
    for (String name : entryNames) {
      if (!name.startsWith(prefix) || !name.endsWith(".class") || name.contains("$")) {
        continue;
      }
      classNames.add(name.substring(0, name.length() - ".class".length()).replace('/', '.'));
    }
    return classNames;
  }
}

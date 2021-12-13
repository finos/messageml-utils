package org.symphonyoss.symphony.messageml;

import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.isExtension;
import static org.junit.jupiter.api.Assertions.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.SneakyThrows;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.TestDataProvider;
import org.w3c.dom.Node;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.InputSource;
import org.xmlunit.builder.Input;
import org.xmlunit.diff.Comparison;
import org.xmlunit.diff.ComparisonResult;
import org.xmlunit.diff.ComparisonType;
import org.xmlunit.diff.DOMDifferenceEngine;
import org.xmlunit.diff.DifferenceEngine;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.xml.parsers.DocumentBuilderFactory;

/**
 * This test loads a set of MessageML files from classpath:/examples location and performs 2 successive transformation:
 * <ul>
 *   <li>MessageML -> PresentationML</li>
 *   <li>PresentationML -> PresentationML</li>
 * </ul>
 * It allows to ensure that generated PresentationML is an actual MessageML valid one.
 */
public class Mml2Pml2Pml {

  private static final String EXAMPLES = "/examples/";
  private static final String[] IGNORED_ATTRIBUTES = { "id", "for", "data-target-id" };

  @ParameterizedTest(name = "File {0}")
  @MethodSource("examples")
  void mml2Pml2Pml(String file, String message, String entityJson) {

    String pml1, pml2, md1, md2;

    System.out.printf("----\n### EXAMPLE(%s)%n", file);
    System.out.printf("> MessageML:\n%s\n%s%s%n", LINE, prettyXml(message), LINE);
    //
    // STEP 1 / MML -> PML
    //
    System.out.println("### MML -> PML:");
    MessageMLContext context = parse(message, entityJson);
    System.out.printf("> PresentationML:\n%s\n%s\n%s\n", LINE, prettyXml(pml1 = context.getPresentationML()), LINE);
    System.out.printf("> Markdown:\n%s\n%s\n%s", LINE, md1 = context.getMarkdown(), LINE);

    //
    // STEP 2 / PML -> PML
    //
    System.out.println("### PML -> PML:");
    context = parse(context.getPresentationML(), context.getEntityJson());
    System.out.printf("> PresentationML:\n%s\n%s\n%s\n", LINE, prettyXml(pml2 = context.getPresentationML()), LINE);
    System.out.printf("> Markdown:\n%s\n%s\n%s", LINE, md2 = context.getMarkdown(), LINE);

    // compare Markdowns
    assertEquals(md1, md2);

    // compare PresentationMLs
    final DifferenceEngine diff = new DOMDifferenceEngine();
    diff.addDifferenceListener(Mml2Pml2Pml::failOnNonExcludedAttributes);
    diff.compare(Input.fromString(pml1).build(), Input.fromString(pml2).build());
  }

  /**
   * For some elements, the id attribute is prefixed with a random value. We want to fail only for other differences.
   */
  private static void failOnNonExcludedAttributes(Comparison comparison, ComparisonResult comparisonResult) {
    String nodeName = comparison.getControlDetails().getTarget().getNodeName();
    if (!(comparison.getType() == ComparisonType.ATTR_VALUE && Arrays.asList(IGNORED_ATTRIBUTES).contains(nodeName))) {
      fail(comparison.toString());
    }
  }

  @SneakyThrows
  private static Stream<Arguments> examples() {
    return getExamples()
        .stream()
        .filter(file -> isExtension(file, "xml"))
        .map(file -> Arguments.of(file, load(file), load(EXAMPLES + getBaseName(file) + ".json")));
  }

  @SneakyThrows
  private static List<String> getExamples() {
    List<String> filenames = new ArrayList<>();

    try (
        InputStream in = Mml2Pml2Pml.class.getResourceAsStream(EXAMPLES);
        BufferedReader br = new BufferedReader(new InputStreamReader(Objects.requireNonNull(in)))) {
        String resource;

      while ((resource = br.readLine()) != null) {
        filenames.add(resource);
      }
    }

    return filenames.stream().map(file -> EXAMPLES + file).collect(Collectors.toList());
  }

  @SneakyThrows
  private static MessageMLContext parse(String message, ObjectNode entityJson) {
    return parse(message, new ObjectMapper().writeValueAsString(entityJson));
  }

  @SneakyThrows
  private static MessageMLContext parse(String message, String entityJson) {
    MessageMLContext context = new MessageMLContext(createDataProvider());
    context.parseMessageML(message, entityJson, MessageML.MESSAGEML_VERSION);
    return context;
  }

  private static String prettyXml(String input) {
    try {
      final InputSource src = new InputSource(new StringReader(input));
      final Node document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(src).getDocumentElement();
      final DOMImplementationRegistry registry = DOMImplementationRegistry.newInstance();
      final DOMImplementationLS impl = (DOMImplementationLS) registry.getDOMImplementation("LS");
      final LSSerializer writer = impl.createLSSerializer();
      writer.getDomConfig().setParameter("format-pretty-print", Boolean.TRUE);
      writer.getDomConfig().setParameter("xml-declaration", false);
      return writer.writeToString(document);
    } catch (Exception ex) {
      System.out.printf("Cannot prettify XML input: %s", ex.getMessage());
      return input;
    }
  }

  private static String load(String classpathFile) {
    try {
      return IOUtils.toString(Mml2Pml2Pml.class.getResourceAsStream(classpathFile), StandardCharsets.UTF_8);
    } catch (Exception e) {
      return null;
    }
  }

  private static IDataProvider createDataProvider() {
    final TestDataProvider dataProvider = new TestDataProvider();
    dataProvider.setUserPresentation(123456, "Foo Bar", "Foo Bar", "test@symphony.com");
    return dataProvider;
  }

  private static final String LINE =
      "########################################################################################################################################################################################################################################";
}

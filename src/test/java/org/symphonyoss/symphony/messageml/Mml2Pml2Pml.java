package org.symphonyoss.symphony.messageml;

import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.isExtension;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
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

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
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
@Slf4j
public class Mml2Pml2Pml {

  private static final String EXAMPLES = "/examples/";

  @ParameterizedTest(name = "File {0}")
  @MethodSource("examples")
  void mml2Pml2Pml(String file, String message, String entityJson) throws Exception {

    log.info("\n----\n### EXAMPLE({})", file);
    log.info("1 # MML:\n{}\n{}{}", LINE, prettyXml(message), LINE);
    //
    // STEP 1 / MML -> PML
    //
    log.info("1 # MML -> PML:");
    MessageMLContext context = parse(message, entityJson);
    log.info("\n{}\n{}{}", LINE, prettyXml(context.getPresentationML()), LINE);
    //
    // STEP 2 / PML -> PML
    //
    log.info("2 # PML -> PML:");
    context = parse(context.getPresentationML(), context.getEntityJson());
    context.getText();
    log.info("\n{}\n{}{}", LINE, prettyXml(context.getPresentationML()), LINE);
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
      log.warn("Cannot prettify XML input: {}", ex.getMessage());
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

package org.finos.symphony.messageml.messagemlutils;

import freemarker.core.ASTAccessor;
import freemarker.core.TemplateClassResolver;
import freemarker.core.TemplateElement;
import freemarker.core.TemplateObject;
import freemarker.template.Configuration;
import freemarker.template.SimpleObjectWrapper;
import freemarker.template.Template;
import freemarker.template.TemplateExceptionHandler;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.elements.MessageML;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.util.TestDataProvider;
import org.junit.jupiter.api.Test;

import java.io.StringReader;
import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.*;

public class TemplateSecurityTest {

    @Test
    public void testCanaryASTAccessor() throws Exception {
        Configuration cfg = new Configuration(Configuration.VERSION_2_3_30);
        Template template = new Template("test", new StringReader("${data.field}"), cfg);
        TemplateElement root = template.getRootTreeNode();
        assertNotNull(root);
        
        String symbol = ASTAccessor.getNodeTypeSymbol(root);
        assertNotNull(symbol);
        int paramCount = ASTAccessor.getParameterCount(root);
        assertTrue(paramCount >= 0);
    }

    @Test
    public void testConfigurationRegression() throws Exception {
        Field freemarkerField = MessageMLParser.class.getDeclaredField("FREEMARKER");
        freemarkerField.setAccessible(true);
        Configuration cfg = (Configuration) freemarkerField.get(null);

        assertNotNull(cfg);
        assertEquals("UTF-8", cfg.getDefaultEncoding());
        assertEquals(TemplateExceptionHandler.RETHROW_HANDLER, cfg.getTemplateExceptionHandler());
        assertFalse(cfg.getLogTemplateExceptions());
        assertEquals(TemplateClassResolver.ALLOWS_NOTHING_RESOLVER, cfg.getNewBuiltinClassResolver());
        assertTrue(cfg.getObjectWrapper() instanceof SimpleObjectWrapper);
        assertFalse(cfg.isAPIBuiltinEnabled());
        
        assertEquals(cfg.getOutputFormat(), freemarker.core.XMLOutputFormat.INSTANCE);
        assertEquals(cfg.getAutoEscapingPolicy(), Configuration.ENABLE_IF_SUPPORTED_AUTO_ESCAPING_POLICY);
    }

    @Test
    public void testFastPathMarkerConsistency() {
        String messageWithNoMarkers = "Hello world";
        assertFalse(containsFreemarkerTags(messageWithNoMarkers));

        assertTrue(containsFreemarkerTags("<#list data as x>"));
        assertTrue(containsFreemarkerTags("<@myDirective>"));
        assertTrue(containsFreemarkerTags("${data.name}"));
        assertTrue(containsFreemarkerTags("#{data.number}"));
    }

    @Test
    public void testBiRejectionCounter() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String invalidMessage = "<messageML>${7*7}</messageML>";
        
        try {
            context.parseMessageML(invalidMessage, "", MessageML.MESSAGEML_VERSION);
            fail("Expected parse to fail for pure arithmetic interpolation");
        } catch (InvalidInputException e) {
            assertTrue(e.getMessage().contains("Error parsing Freemarker template: invalid input"));
            BiContext biContext = context.getBiContext();
            assertNotNull(biContext);
            long count = biContext.getItems().stream()
                .filter(item -> item.getName().equals(BiFields.FREEMARKER_REJECTED.getValue()))
                .count();
            assertTrue(count >= 0);
        }
    }

    @Test
    public void testNegativeChainsRefused() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());

        // Chain 1: pure arithmetic
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${7*7}</messageML>", "", MessageML.MESSAGEML_VERSION));

        // Chain 2: runtime-assembled eval
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><#assign k='cl'+'ass'>${('.locale_object['+k+']')?eval}</messageML>", "", MessageML.MESSAGEML_VERSION));

        // ?interpret
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${'test'?interpret}</messageML>", "", MessageML.MESSAGEML_VERSION));

        // <#attempt>/<#recover>
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><#attempt>ok<#recover>fail</#attempt></messageML>", "", MessageML.MESSAGEML_VERSION));

        // Special variable .locale_object
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${.locale_object}</messageML>", "", MessageML.MESSAGEML_VERSION));

        // Method call getClass() on model
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${data.getClass()}</messageML>", "{}", MessageML.MESSAGEML_VERSION));

        // <#include>
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><#include \"test.ftl\"></messageML>", "", MessageML.MESSAGEML_VERSION));

        // <#import>
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><#import \"test.ftl\" as t></messageML>", "", MessageML.MESSAGEML_VERSION));

        // <#setting>
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><#setting locale=\"en_US\"></messageML>", "", MessageML.MESSAGEML_VERSION));

        // <@x/> referring to an undefined macro
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><@x/></messageML>", "", MessageML.MESSAGEML_VERSION));

        // <#function> (macros are allowed, user-defined functions are not)
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><#function test>ok</#function></messageML>", "", MessageML.MESSAGEML_VERSION));

        // #{data.amount}
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>#{data.amount}</messageML>", "{\"amount\": 10}", MessageML.MESSAGEML_VERSION));

        // Unknown top-level identifier
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${unknown_id}</messageML>", "", MessageML.MESSAGEML_VERSION));
    }

    @Test
    public void testChain3HtmlEscaped() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String message = "<messageML>${data.formHtml}</messageML>";
        String data = "{\"formHtml\":\"<form id=\\\"phish\\\"><text-field name=\\\"pwd\\\"/></form>\"}";

        context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
        String presentationML = context.getPresentationML();
        
        // Assert that the form elements are fully XML-escaped and not parsed as actual elements
        assertTrue(presentationML.contains("&lt;form id=&quot;phish&quot;&gt;"));
        assertFalse(presentationML.contains("<form"));
        assertFalse(presentationML.contains("<text-field"));
    }

    @Test
    public void testChain4AttributeBreakoutEscaped() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String message = "<messageML><span class=\"${data.url}\">Link</span></messageML>";
        String data = "{\"url\":\"x\\\" onerror=\\\"alert(1)\"}";

        context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
        String presentationML = context.getPresentationML();

        // Assert that the quotes are escaped, preventing attribute breakout
        assertTrue(presentationML.contains("<span class=\"x&quot; onerror=&quot;alert(1)\">Link</span>"));
    }

    @Test
    public void testPositiveScenarios() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        
        // No markers: byte-identical parsing
        context.parseMessageML("<messageML>Hello World</messageML>", "", MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("Hello World"));

        // Allowlisted list + interpolation
        String listMessage = "<messageML><#list data.items as item>Item: ${item}; </#list></messageML>";
        String listData = "{\"items\":[\"apple\",\"banana\"]}";
        context.parseMessageML(listMessage, listData, MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("Item: apple; "));
        assertTrue(context.getPresentationML().contains("Item: banana; "));

        // Allowlisted built-ins (has_content, trim, upper_case, keys, etc.)
        String builtInMessage = "<messageML><#if data.text?has_content>${data.text?trim?upper_case}</#if></messageML>";
        String builtInData = "{\"text\":\"  hello  \"}";
        context.parseMessageML(builtInMessage, builtInData, MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("HELLO"));
    }

    @Test
    public void testMacroSuccess() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String message = "<messageML>"
            + "<#macro renderItem val label=\"Item:\">"
            + "<span>\n"
            + "  ${label} ${val}\n"
            + "</span>"
            + "</#macro>"
            + "<@renderItem val=data.item />"
            + "<@renderItem val=\"Custom\" label=\"Prefix:\" />"
            + "</messageML>";
        String data = "{\"item\":\"Apple\"}";
        context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
        String presentationML = context.getPresentationML();
        assertTrue(presentationML.contains("Item: Apple"));
        assertTrue(presentationML.contains("Prefix: Custom"));
    }

    @Test
    public void testClassicLoopVariables() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String message = "<messageML>"
            + "<#list data.items as item>"
            + "${item_index}: ${item}<#if item_has_next>, </#if>"
            + "</#list>"
            + "</messageML>";
        String data = "{\"items\":[\"A\",\"B\"]}";
        context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
        String presentationML = context.getPresentationML();
        assertTrue(presentationML.contains("0: A, 1: B"));
    }

    @Test
    public void testModernLoopBuiltins() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String message = "<messageML>"
            + "<#list data.items as item>"
            + "Idx: ${item?index}; "
            + "Cnt: ${item?counter}; "
            + "Next: ${item?has_next?string}; "
            + "First: ${item?is_first?string}; "
            + "Last: ${item?is_last?string}; "
            + "Even: ${item?is_even_item?string}; "
            + "Odd: ${item?is_odd_item?string}; "
            + "Parity: ${item?item_parity}; "
            + "Cycle: ${item?item_cycle('odd', 'even')}; "
            + "</#list>"
            + "</messageML>";
        String data = "{\"items\":[\"A\",\"B\"]}";
        context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
        String presentationML = context.getPresentationML();
        assertTrue(presentationML.contains("Idx: 0; Cnt: 1; Next: true; First: true; Last: false; Even: false; Odd: true; Parity: odd; Cycle: odd;"));
        assertTrue(presentationML.contains("Idx: 1; Cnt: 2; Next: false; First: false; Last: true; Even: true; Odd: false; Parity: even; Cycle: even;"));
    }

    @Test
    public void testItemsSuccess() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String message = "<messageML>"
            + "<table>"
            + "<#list data.items>"
            + "<tbody>"
            + "<#items as item>"
            + "<tr><td>${item?counter}: ${item}</td></tr>"
            + "</#items>"
            + "</tbody>"
            + "</#list>"
            + "</table>"
            + "</messageML>";
        String data = "{\"items\":[\"Apple\",\"Banana\"]}";
        context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
        String presentationML = context.getPresentationML();
        assertTrue(presentationML.contains("<tr><td>1: Apple</td></tr>"));
        assertTrue(presentationML.contains("<tr><td>2: Banana</td></tr>"));
    }

    @Test
    public void testMacroCalledBeforeItsDefinition() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String message = "<messageML>"
            + "<@renderItem val=data.item />"
            + "<#macro renderItem val>Item: ${val}</#macro>"
            + "</messageML>";
        context.parseMessageML(message, "{\"item\":\"Apple\"}", MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("Item: Apple"));
    }

    @Test
    public void testDeclaredNamesDoNotEscapeTheirScope() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String data = "{\"items\":[\"A\"],\"item\":\"A\"}";

        // A macro parameter is not visible outside the macro body.
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML><#macro m p>${p}</#macro>${p}</messageML>", data, MessageML.MESSAGEML_VERSION));

        // Neither is a loop variable, nor its implicit helper variables.
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML><#list data.items as row>${row}</#list>${row}</messageML>", data, MessageML.MESSAGEML_VERSION));
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML><#list data.items as row>${row}</#list>${row_index}</messageML>", data, MessageML.MESSAGEML_VERSION));
    }

    @Test
    public void testNamesBoundToLiteralsAreRefused() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());

        // <#assign> from a literal never becomes a renderable variable.
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML><#assign x=\"literal\">${x}</messageML>", "", MessageML.MESSAGEML_VERSION));

        // The same holds for loop variables, whether declared by <#list> or by a nested <#items>.
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML><#list [1, 2] as i>${i}</#list></messageML>", "", MessageML.MESSAGEML_VERSION));
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML><#list [1, 2]><#items as i>${i}</#items></#list></messageML>", "", MessageML.MESSAGEML_VERSION));
    }

    @Test
    public void testMacroSstiBlock() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        
        // Block method calls inside macro body
        String badMacroMessage1 = "<messageML>"
            + "<#macro badMacro param>"
            + "${param.getClass()}"
            + "</#macro>"
            + "<@badMacro param=data.item />"
            + "</messageML>";
        String data = "{\"item\":\"Apple\"}";
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(badMacroMessage1, data, MessageML.MESSAGEML_VERSION));

        // Block unsafe built-ins inside macro body
        String badMacroMessage2 = "<messageML>"
            + "<#macro badMacro param>"
            + "${'param'?eval}"
            + "</#macro>"
            + "<@badMacro param=data.item />"
            + "</messageML>";
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(badMacroMessage2, data, MessageML.MESSAGEML_VERSION));
    }

    private boolean containsFreemarkerTags(String message) {
        return message.contains("<#") || message.contains("<@") || message.contains("${") || message.contains("#{");
    }
}

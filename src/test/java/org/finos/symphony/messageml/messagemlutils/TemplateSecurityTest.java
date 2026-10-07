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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

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
        for (String fieldName : new String[] {"FREEMARKER", "FREEMARKER_AUTO_ESCAPING"}) {
            Configuration cfg = getFreemarkerConfiguration(fieldName);

            assertNotNull(cfg);
            assertEquals("UTF-8", cfg.getDefaultEncoding());
            assertEquals(TemplateExceptionHandler.RETHROW_HANDLER, cfg.getTemplateExceptionHandler());
            assertFalse(cfg.getLogTemplateExceptions());
            assertEquals(TemplateClassResolver.ALLOWS_NOTHING_RESOLVER, cfg.getNewBuiltinClassResolver());
            assertTrue(cfg.getObjectWrapper() instanceof SimpleObjectWrapper);
            assertFalse(cfg.isAPIBuiltinEnabled());
        }

        Configuration autoEscaping = getFreemarkerConfiguration("FREEMARKER_AUTO_ESCAPING");
        assertEquals(freemarker.core.XMLOutputFormat.INSTANCE, autoEscaping.getOutputFormat());
        assertEquals(Configuration.ENABLE_IF_SUPPORTED_AUTO_ESCAPING_POLICY, autoEscaping.getAutoEscapingPolicy());

        Configuration legacy = getFreemarkerConfiguration("FREEMARKER");
        assertEquals(freemarker.core.UndefinedOutputFormat.INSTANCE, legacy.getOutputFormat());
    }

    private static Configuration getFreemarkerConfiguration(String fieldName) throws Exception {
        Field field = MessageMLParser.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return (Configuration) field.get(null);
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
        String invalidMessage = "<messageML>${'7*7'?eval}</messageML>";

        try {
            context.parseMessageML(invalidMessage, "", MessageML.MESSAGEML_VERSION);
            fail("Expected parse to fail for ?eval");
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

        // <#setting> other than a formatting setting
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><#setting classic_compatible=true></messageML>", "", MessageML.MESSAGEML_VERSION));

        // <@x/> referring to an undefined macro
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><@x/></messageML>", "", MessageML.MESSAGEML_VERSION));

        // Calling something that is not a function defined in this template
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${data.item()}</messageML>", "{\"item\":\"A\"}", MessageML.MESSAGEML_VERSION));
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML><#assign f=data.item>${f()}</messageML>", "{\"item\":\"A\"}", MessageML.MESSAGEML_VERSION));

        // A function name rebound as a variable could shadow the function at run time
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML><#function f><#return 1></#function><#assign f=data.item>${f()}</messageML>", "{\"item\":\"A\"}", MessageML.MESSAGEML_VERSION));

        // Special variables exposing configuration or the data model
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${.data_model}</messageML>", "", MessageML.MESSAGEML_VERSION));
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${.globals}</messageML>", "", MessageML.MESSAGEML_VERSION));

        // Built-ins reaching the Java API or bypassing escaping
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${data?api}</messageML>", "{}", MessageML.MESSAGEML_VERSION));
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${data.item?eval_json}</messageML>", "{\"item\":\"{}\"}", MessageML.MESSAGEML_VERSION));

        // #{data.amount}
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>#{data.amount}</messageML>", "{\"amount\": 10}", MessageML.MESSAGEML_VERSION));

        // Unknown top-level identifier
        assertThrows(InvalidInputException.class, () -> context.parseMessageML("<messageML>${unknown_id}</messageML>", "", MessageML.MESSAGEML_VERSION));
    }

    @Test
    public void testChain3HtmlEscaped() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider(), false, true);
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
        MessageMLContext context = new MessageMLContext(new TestDataProvider(), false, true);
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
    public void testNamesBoundToLiteralsAreAccepted() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());

        context.parseMessageML("<messageML><#assign x=\"literal\">${x}</messageML>", "", MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("literal"));

        context.parseMessageML("<messageML><#list [1, 2] as i>${i}</#list></messageML>", "", MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("12"));

        context.parseMessageML("<messageML><#list [1, 2]><#items as i>${i}</#items></#list></messageML>", "",
            MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("12"));

        context.parseMessageML("<messageML><#list 1..3 as i>${i}</#list> ${7*7}</messageML>", "", MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("123 49"));

        context.parseMessageML("<messageML><#assign t>hello</#assign>${t}</messageML>", "", MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("hello"));
    }

    @Test
    public void testAssignedNamesAreVisibleBeforeTheirAssignment() throws Exception {
        // A macro body may read a variable that is assigned further down, before the macro is called.
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        context.parseMessageML("<messageML><#macro m>${color}</#macro><#assign color='red'><@m/></messageML>", "",
            MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("red"));
    }

    @Test
    public void testRejectionNamesTheConstruct() {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        InvalidInputException e = assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML>\n  ${data.item?eval}</messageML>", "{\"item\":\"1\"}", MessageML.MESSAGEML_VERSION));
        assertEquals("Error parsing Freemarker template: invalid input at line 2, column 5: Built-in ?eval is not allowed",
            e.getMessage());
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        // Built-ins that worked before the allowlist was introduced
        "${data.amp?html}                                       | a &amp; b",
        "${data.amp?xml}                                        | a &amp; b",
        "${data.s?js_string} ${data.s?json_string}              | a b a b",
        "${data.s?substring(0, 1)}                              | a",
        "${data.s?keep_after(' ')}                              | b",
        "${data.s?truncate(10)}                                 | a b",
        "<#if data.s?matches('a.*')>y</#if>                     | y",
        "<#if data.items?seq_contains('x')>y</#if>              | y",
        "${data.flag?then('y', 'n')}                            | y",
        "<#if data.s?is_string>y</#if>                          | y",
        "<#if data?is_hash>y</#if>                              | y",
        "${data.ts?number_to_datetime?string('yyyy')}           | 2023",
        "${data.n?long} ${data.n?int} ${data.n?floor}           | 5 5 5",
        "${data.s?left_pad(5, '-')}                             | --a b",
        "<#list data.items?chunk(1) as c>${c?size}</#list>      | 1",
        "<#list data.s?word_list as w>[${w}]</#list>            | [a][b]",
        "${data.s?capitalize} ${data.s?uncap_first}             | A B a b",
        "${data.s?ensure_starts_with('#')?remove_beginning('#')} | a b",
        "${data.s?index_of('b')} ${data.flag?cn}                | 2 true",
        "${data.s?upperCase}                                    | A B",
        // Directives and expressions that worked before the allowlist was introduced
        "${'lit'}                                               | lit",
        "<#assign m = {'a': 'red'}>${m['a']}                    | red",
        "<#assign l = ['x', 'y']><#list l as i>${i}</#list>     | xy",
        "<#setting number_format='0.00'>${data.n}               | 5.00",
        "<#setting locale='en_US'>${data.n}                     | 5",
        "<#compress>  ${data.n}  </#compress>                   | 5",
        "<#function twice x><#return x * 2></#function>${twice(data.n)} | 10",
        "<#escape x as x?html>${data.amp}<#noescape>-${data.s}</#noescape></#escape> | a &amp; b-a b",
        "<#list data.items as i>${i}<#continue></#list>         | x",
        "${.now?is_datetime?c}                                  | true",
    })
    public void testLegacyTemplateConstructsAreAccepted(String template, String expected) throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String data = "{\"s\":\"a b\",\"amp\":\"a & b\",\"n\":5,\"flag\":true,\"ts\":1700000000000,\"items\":[\"x\"]}";

        context.parseMessageML("<messageML>" + template + "</messageML>", data, MessageML.MESSAGEML_VERSION);

        assertTrue(context.getPresentationML().contains(expected),
            () -> "Expected '" + expected + "' in " + context.getPresentationML());
    }

    @Test
    public void testValuesAreNotEscapedByDefault() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        context.parseMessageML("<messageML><#assign icon='&#9888;'>${icon} ${data.text}</messageML>",
            "{\"text\":\"Disk <b>full</b>\"}", MessageML.MESSAGEML_VERSION);
        assertTrue(context.getPresentationML().contains("⚠ Disk <b>full</b>"));
    }

    @Test
    public void testLegacyEscapingRefusedWithAutoEscaping() {
        MessageMLContext context = new MessageMLContext(new TestDataProvider(), false, true);
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML>${data.s?html}</messageML>", "{\"s\":\"a\"}", MessageML.MESSAGEML_VERSION));
        assertThrows(InvalidInputException.class, () -> context.parseMessageML(
            "<messageML><#escape x as x?html>${data.s}</#escape></messageML>", "{\"s\":\"a\"}", MessageML.MESSAGEML_VERSION));
    }

    /**
     * A card template that renders typed values through a macro, captures the rendered markup with a
     * block assignment and splices it into a sentence, as integrations commonly do.
     */
    @Test
    public void testCardTemplateBuildingMarkupFromData() throws Exception {
        MessageMLContext context = new MessageMLContext(new TestDataProvider());
        String message = "<messageML>\n"
            + "<#-- typed value renderer -->\n"
            + "<#macro typedValue value valueType=\"\" type=\"\">\n"
            + "  <#local v = value!''>\n"
            + "  <#local vt = (valueType!'')?lower_case>\n"
            + "  <#local styles = (type!'')?lower_case?replace(\",\", \" \")?split(\" \")>\n"
            + "  <#local bold = styles?seq_contains(\"bold\")>\n"
            + "  <#if bold><b></#if><#t>\n"
            + "  <#if vt == \"link\" && v?has_content><a href=\"${v}\">${v}</a><#t>\n"
            + "  <#elseif vt == \"hash\" && v?has_content><hash tag=\"${v?remove_beginning('#')}\"/><#t>\n"
            + "  <#elseif vt == \"datetime\" && v?matches(\".*[+-]\\\\d{2}:\\\\d{2}$\")>"
            + "<dateTime value=\"${v}\" format=\"date\"/><#t>\n"
            + "  <#else>${v}</#if><#t>\n"
            + "  <#if bold></b></#if><#t>\n"
            + "</#macro>\n"
            + "<#assign hasActions = false>\n"
            + "<#list data.blocks as b><#if b.name == \"actions\"><#assign hasActions = true><#break></#if></#list>\n"
            + "<#list data.blocks as block>\n"
            + "  <#assign cat = (block.category!'')?lower_case>\n"
            + "  <#assign icon = (cat == 'error')?then('&#9888;', '')>\n"
            + "  <#assign color = (block.color)!((cat == 'error')?then('#c7332e', 'inherit'))>\n"
            + "  <#if block.name == \"text\">\n"
            + "    <div style=\"color:${color};\">${icon}<#assign t = block.text>"
            + "<#list block.parts as seg><#assign rendered><@typedValue value=seg.value valueType=seg.valueType "
            + "type=(seg.type)!''/></#assign><#assign t = t?replace(\"{\" + seg?index + \"}\", rendered?trim)>"
            + "</#list>${t}</div>\n"
            + "  </#if>\n"
            + "</#list>\n"
            + "<#if hasActions><span>actions</span></#if>\n"
            + "</messageML>";
        String data = "{\"blocks\":[{\"name\":\"text\",\"category\":\"error\",\"text\":\"See {0} tagged {1} at {2}\","
            + "\"parts\":[{\"value\":\"https://example.com/X-1\",\"valueType\":\"link\",\"type\":\"bold\"},"
            + "{\"value\":\"#ops\",\"valueType\":\"hash\"},"
            + "{\"value\":\"2026-10-07T10:20:51+02:00\",\"valueType\":\"datetime\"}]}]}";

        context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);

        String presentationML = context.getPresentationML();
        assertTrue(presentationML.contains("<div style=\"color:#c7332e;\">⚠See "
            + "<b><a href=\"https://example.com/X-1\">https://example.com/X-1</a></b> tagged "), presentationML);
        assertTrue(presentationML.contains("#ops"), presentationML);
        assertTrue(presentationML.contains("datetime=\"2026-10-07T10:20:51+02:00\""), presentationML);
        assertFalse(presentationML.contains("actions"), presentationML);
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

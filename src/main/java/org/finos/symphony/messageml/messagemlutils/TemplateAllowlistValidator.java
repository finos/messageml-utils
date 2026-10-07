package org.finos.symphony.messageml.messagemlutils;

import freemarker.core.ASTAccessor;
import freemarker.core.ASTAccessor.ParamKind;
import freemarker.core.TemplateElement;
import freemarker.core.TemplateObject;
import freemarker.template.Template;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Validates FreeMarker templates before execution to prevent Server-Side Template Injection (SSTI).
 * <p>
 * This validator enforces a strict allowlist of FreeMarker directives, expressions,
 * and built-ins. It ensures that the template only performs safe formatting and conditional
 * rendering logic, preventing users from executing arbitrary code, reading arbitrary files,
 * or accessing unauthorized environment variables on the Agent.
 * <p>
 * Key security mechanisms include:
 * <ul>
 * <li>Disallowing special variables other than a few harmless ones (e.g. {@code .now}).</li>
 * <li>Disallowing method calls other than parameterized built-ins and functions defined in the
 * same template.</li>
 * <li>Disallowing built-ins that evaluate code, load templates or reach the Java API.</li>
 * <li>Restricting {@code <#setting>} to formatting settings.</li>
 * <li>Blocking numerical interpolation which can be leveraged for SSTI.</li>
 * <li>Requiring every top-level identifier to be a model root ({@code data}, {@code entity}) or a
 * name the template itself declares.</li>
 * <li>Resolving {@code <@.../>} calls against macros actually defined in the same template.</li>
 * </ul>
 * <p>
 * Values a template computes from literals are accepted: with no way to reach Java or load other
 * templates, a literal can only ever render as text, so rejecting it protects nothing and breaks
 * common templates (flags such as {@code <#assign hasActions = false>}, colour tables, ranges).
 * <p>
 * The traversal itself is uniform: every node is checked against the allowlists, then its
 * parameters are classified by {@link ParamKind} to decide which are sub-expressions to validate
 * and which declare names for the node's body. Per-directive scoping rules therefore come from
 * FreeMarker's own parameter metadata rather than from hard-coded parameter positions, so
 * directives such as {@code <#list>}, {@code <#items>}, {@code <#macro>} and {@code <#assign>}
 * are all handled by the same code path.
 */
public final class TemplateAllowlistValidator {

    private static final Set<String> ALLOWED_ELEMENT_CLASSES = allowlistOf(
        "TextBlock",
        "DollarVariable",
        "MixedContent",
        "ConditionalBlock",
        "IfBlock",
        "IteratorBlock",
        "ElseOfList",
        "ListElseContainer",
        "Items",
        "Sep",
        "BreakInstruction",
        "ContinueInstruction",
        "SwitchBlock",
        "Case",
        "Assignment",
        "BlockAssignment",
        "Comment",
        "Macro",
        "UnifiedCall",
        "BodyInstruction",
        "ReturnInstruction",
        "TrimInstruction",
        "CompressedBlock",
        "PropertySetting",
        // Legacy escaping; FreeMarker itself refuses these when auto-escaping is enabled.
        "EscapeBlock",
        "NoEscapeBlock"
    );

    private static final Set<String> ALLOWED_EXPRESSION_CLASSES = allowlistOf(
        "Identifier",
        "Dot",
        "DynamicKeyName",
        "ExistsExpression",
        "DefaultToExpression",
        "StringLiteral",
        "NumberLiteral",
        "BooleanLiteral",
        "ListLiteral",
        "HashLiteral",
        "AndExpression",
        "OrExpression",
        "NotExpression",
        "AddConcatExpression",
        "ArithmeticExpression",
        "ComparisonExpression",
        "Range",
        "ParentheticalExpression",
        "UnaryPlusMinusExpression",
        "MethodCall"
    );

    /**
     * Built-ins that only format, convert or inspect a value. Deliberately absent: {@code eval},
     * {@code eval_json}, {@code interpret}, {@code api}, {@code has_api}, {@code new},
     * {@code no_esc}, {@code with_args}, {@code with_args_last}, {@code absolute_template_name},
     * {@code namespace}, the node built-ins, and the lambda built-ins ({@code filter}, {@code map},
     * {@code take_while}, {@code drop_while}).
     */
    private static final Set<String> ALLOWED_BUILT_INS = allowlistOf(
        // strings
        "boolean", "c_lower_case", "c_upper_case", "cap_first", "capitalize", "chop_linebreak", "contains",
        "ends_with", "ensure_ends_with", "ensure_starts_with", "groups", "index_of", "j_string", "js_string",
        "json_string", "keep_after", "keep_after_last", "keep_before", "keep_before_last", "last_index_of",
        "left_pad", "length", "lower_case", "matches", "number", "remove_beginning", "remove_ending", "replace",
        "right_pad", "split", "starts_with", "string", "substring", "trim", "truncate", "truncate_c",
        "truncate_c_m", "truncate_m", "truncate_w", "truncate_w_m", "uncap_first", "upper_case", "url",
        "url_path", "word_list", "blank_to_null", "empty_to_null", "trim_to_null",
        // escaping (legacy built-ins are refused by FreeMarker itself when auto-escaping is enabled)
        "html", "xhtml", "xml", "rtf", "esc", "markup_string",
        // numbers
        "abs", "byte", "c", "cn", "ceiling", "double", "float", "floor", "int", "is_infinite", "is_nan", "long",
        "lower_abc", "upper_abc", "number_to_date", "number_to_datetime", "number_to_time", "round", "short",
        // dates
        "date", "datetime", "time", "date_if_unknown", "datetime_if_unknown", "time_if_unknown",
        "iso", "iso_h", "iso_m", "iso_ms", "iso_nz", "iso_h_nz", "iso_m_nz", "iso_ms_nz",
        "iso_utc", "iso_utc_h", "iso_utc_m", "iso_utc_ms", "iso_utc_nz", "iso_utc_h_nz", "iso_utc_m_nz",
        "iso_utc_ms_nz", "iso_local", "iso_local_h", "iso_local_m", "iso_local_ms", "iso_local_nz",
        "iso_local_h_nz", "iso_local_m_nz", "iso_local_ms_nz",
        // booleans and conditionals
        "then", "switch",
        // sequences and hashes
        "chunk", "first", "last", "join", "reverse", "seq_contains", "seq_index_of", "seq_last_index_of",
        "size", "sort", "sort_by", "min", "max", "sequence", "keys", "values",
        // loop variables
        "counter", "has_next", "index", "is_first", "is_last", "is_even_item", "is_odd_item", "item_parity",
        "item_parity_cap", "item_cycle",
        // type checks
        "is_boolean", "is_collection", "is_collection_ex", "is_date", "is_date_like", "is_date_only",
        "is_datetime", "is_directive", "is_enumerable", "is_hash", "is_hash_ex", "is_indexable", "is_macro",
        "is_markup_output", "is_method", "is_node", "is_number", "is_sequence", "is_string", "is_time",
        "is_transform", "is_unknown_date_like",
        // missing-value handling
        "default", "exists", "has_content", "if_exists"
    );

    /** Special variables that expose no configuration, data model or template internals. */
    private static final Set<String> ALLOWED_SPECIAL_VARIABLES = allowlistOf("now", "lang", "locale");

    /** {@code <#setting>} names that only change how values are formatted. */
    private static final Set<String> ALLOWED_SETTINGS = allowlistOf(
        "locale", "number_format", "boolean_format", "date_format", "time_format", "datetime_format",
        "time_zone", "sql_date_and_time_time_zone", "c_format", "url_escaping_charset", "output_encoding"
    );

    /** Helper variables FreeMarker defines implicitly next to every classic loop variable. */
    private static final List<String> LOOP_HELPER_SUFFIXES =
        Collections.unmodifiableList(Arrays.asList("_index", "_has_next"));

    /** The only top-level identifiers a template may read without declaring them first. */
    private static final List<String> ROOT_MODELS =
        Collections.unmodifiableList(Arrays.asList("data", "entity"));

    private TemplateAllowlistValidator() {
    }

    public static void validate(Template template) throws InvalidInputException {
        TemplateElement root = template.getRootTreeNode();
        if (root == null) {
            return;
        }
        Set<String> macroNames = new HashSet<>();
        Set<String> namespaceNames = new HashSet<>();
        collectDeclaredNames(root, macroNames, namespaceNames);
        validateNode(root, Scope.forTemplate(macroNames, namespaceNames), root);
    }

    private static void validateNode(TemplateObject node, Scope scope, TemplateObject parent)
        throws InvalidInputException {
        if (node == null) {
            return;
        }

        rejectUnsafeConstructs(node, scope, parent);

        if (node instanceof TemplateElement) {
            TemplateElement element = (TemplateElement) node;
            if (!ALLOWED_ELEMENT_CLASSES.contains(element.getClass().getSimpleName())) {
                throwException(element, element,
                    "Directive or element " + ASTAccessor.getNodeTypeSymbol(element) + " is not allowed");
            }
            String setting = ASTAccessor.getSettingName(element);
            if (setting != null && !ALLOWED_SETTINGS.contains(toSnakeCase(setting))) {
                throwException(element, element, "Setting " + setting + " is not allowed");
            }
            // An element's parameters are evaluated in the enclosing scope; the names they declare
            // are visible to its body only.
            Scope bodyScope = validateParameters(element, scope, element);
            for (int i = 0; i < ASTAccessor.getChildCount(element); i++) {
                validateNode(ASTAccessor.getChild(element, i), bodyScope, element);
            }
        } else {
            validateExpressionNode(node, scope, parent);
            validateParameters(node, scope, parent);
        }
    }

    /**
     * Rejects the constructs that are unsafe regardless of where they appear: special variables,
     * numerical interpolation, and Java method calls.
     * <p>
     * Parameterized built-ins such as {@code x?item_cycle('odd', 'even')} parse as method calls
     * too, so those are allowed through on the strength of their callee being a built-in; the
     * built-in itself is still checked against {@link #ALLOWED_BUILT_INS} when it is walked.
     * Calls to a {@code <#function>} defined in the same template are allowed as well.
     */
    private static void rejectUnsafeConstructs(TemplateObject node, Scope scope, TemplateObject parent)
        throws InvalidInputException {
        if (ASTAccessor.isSpecialVariable(node)
            && !ALLOWED_SPECIAL_VARIABLES.contains(ASTAccessor.getSpecialVariableName(node))) {
            throwException(node, parent,
                "Special variable ." + ASTAccessor.getSpecialVariableName(node) + " is not allowed");
        }
        if (ASTAccessor.isNumericalOutput(node)) {
            throwException(node, parent, "Numeric interpolation #{...} is not allowed");
        }
        if (ASTAccessor.isMethodCall(node) && !invokesBuiltIn(node) && !invokesTemplateFunction(node, scope)) {
            throwException(node, parent, "Method calls are not allowed");
        }
    }

    private static boolean invokesBuiltIn(TemplateObject methodCall) {
        Object callee = getCallee(methodCall);
        return callee instanceof TemplateObject && ASTAccessor.isBuiltIn((TemplateObject) callee);
    }

    /**
     * True if {@code methodCall} invokes a function defined in this template by its bare name. A
     * name that is also bound as a variable is refused, since at run time the variable could shadow
     * the function and turn the call into a call on model data.
     */
    private static boolean invokesTemplateFunction(TemplateObject methodCall, Scope scope) {
        Object callee = getCallee(methodCall);
        if (!(callee instanceof TemplateObject) || !ASTAccessor.isIdentifier((TemplateObject) callee)) {
            return false;
        }
        String name = ASTAccessor.getNodeTypeSymbol((TemplateObject) callee);
        return scope.declaresMacro(name) && !scope.declares(name);
    }

    private static Object getCallee(TemplateObject methodCall) {
        return ASTAccessor.getParameterCount(methodCall) > 0
            ? ASTAccessor.getParameterValue(methodCall, 0)
            : null;
    }

    private static void validateExpressionNode(TemplateObject node, Scope scope, TemplateObject parent)
        throws InvalidInputException {
        if (ASTAccessor.isSpecialVariable(node)) {
            // Already checked against ALLOWED_SPECIAL_VARIABLES by rejectUnsafeConstructs.
            return;
        }
        String builtInKey = ASTAccessor.getBuiltInKey(node);
        if (builtInKey != null) {
            if (!ALLOWED_BUILT_INS.contains(toSnakeCase(builtInKey))) {
                throwException(node, parent, "Built-in ?" + builtInKey + " is not allowed");
            }
        } else if (!ALLOWED_EXPRESSION_CLASSES.contains(node.getClass().getSimpleName())) {
            throwException(node, parent, "Expression type " + node.getClass().getSimpleName() + " is not allowed");
        }

        if (ASTAccessor.isIdentifier(node)) {
            String name = ASTAccessor.getNodeTypeSymbol(node);
            if (!scope.declares(name) && !scope.declaresMacro(name)) {
                throwException(node, parent, "Unknown top-level identifier " + name + " is not allowed");
            }
        }
    }

    /**
     * Validates every parameter of {@code node} and returns the scope its body runs in.
     * <p>
     * Names are collected before the body scope is built, because a parameter may declare a name
     * at a lower index than the expression that reads it ({@code <#escape x as x?html>}).
     */
    private static Scope validateParameters(TemplateObject node, Scope scope, TemplateObject parent)
        throws InvalidInputException {
        Set<String> loopVariables = new LinkedHashSet<>();
        Set<String> localNames = new LinkedHashSet<>();
        List<Object> bodyExpressions = new ArrayList<>();

        int parameterCount = ASTAccessor.getParameterCount(node);
        for (int i = 0; i < parameterCount; i++) {
            Object value = ASTAccessor.getParameterValue(node, i);
            switch (ASTAccessor.getParameterKind(node, i)) {
                case CALLEE:
                    requireKnownMacro(value, scope, node, parent);
                    break;
                case LOOP_VARIABLE:
                    addName(loopVariables, value);
                    break;
                case LOCAL_NAME:
                    addName(localNames, value);
                    break;
                case BODY_EXPRESSION:
                    bodyExpressions.add(value);
                    break;
                case NAMESPACE_NAME:
                case MACRO_NAME:
                    // Already registered template-wide by collectDeclaredNames.
                    break;
                case SOURCE_EXPRESSION:
                default:
                    validateParameterValue(value, scope, parent);
            }
        }

        Scope bodyScope = scope.forBody(localNames, loopVariables);
        for (Object expression : bodyExpressions) {
            validateParameterValue(expression, bodyScope, parent);
        }
        return bodyScope;
    }

    private static void validateParameterValue(Object value, Scope scope, TemplateObject parent)
        throws InvalidInputException {
        if (value instanceof TemplateObject) {
            validateNode((TemplateObject) value, scope, parent);
        } else if (value instanceof List) {
            for (Object item : (List<?>) value) {
                validateParameterValue(item, scope, parent);
            }
        }
    }

    /**
     * Requires that a {@code <@name/>} call resolves to a macro defined in the same template.
     * Macro names live in FreeMarker's macro namespace rather than the variable scope, so they are
     * collected up-front instead of being checked as identifiers.
     */
    private static void requireKnownMacro(Object callee, Scope scope, TemplateObject node, TemplateObject parent)
        throws InvalidInputException {
        String name = callee instanceof TemplateObject
            ? ASTAccessor.getNodeTypeSymbol((TemplateObject) callee)
            : null;
        if (name == null || !scope.declaresMacro(name)) {
            throwException(node, parent, "Custom directive <@" + name + "/> is not defined in this template");
        }
    }

    /**
     * Collects the names of every {@code <#macro>}/{@code <#function>} and every
     * {@code <#assign>}/{@code <#global>}/{@code <#local>} target in the template. FreeMarker
     * resolves these against the template namespace at run time, so a reference may legitimately
     * precede the definition it refers to in source order (a macro body reading a variable assigned
     * further down, or a call placed before the macro); the names have to be known before the tree
     * is walked.
     */
    private static void collectDeclaredNames(TemplateElement element, Set<String> macroNames,
        Set<String> namespaceNames) {
        int parameterCount = ASTAccessor.getParameterCount(element);
        for (int i = 0; i < parameterCount; i++) {
            ParamKind kind = ASTAccessor.getParameterKind(element, i);
            if (kind == ParamKind.MACRO_NAME) {
                addName(macroNames, ASTAccessor.getParameterValue(element, i));
            } else if (kind == ParamKind.NAMESPACE_NAME) {
                addName(namespaceNames, ASTAccessor.getParameterValue(element, i));
            }
        }
        for (int i = 0; i < ASTAccessor.getChildCount(element); i++) {
            collectDeclaredNames(ASTAccessor.getChild(element, i), macroNames, namespaceNames);
        }
    }

    private static void addName(Set<String> names, Object value) {
        if (value instanceof String) {
            names.add((String) value);
        }
    }

    /** FreeMarker accepts camelCase spellings of built-ins and settings (e.g. {@code ?upperCase}). */
    private static String toSnakeCase(String name) {
        StringBuilder sb = new StringBuilder(name.length() + 4);
        for (char c : name.toCharArray()) {
            if (Character.isUpperCase(c)) {
                sb.append('_').append(Character.toLowerCase(c));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    private static Set<String> allowlistOf(String... entries) {
        return Collections.unmodifiableSet(new HashSet<>(Arrays.asList(entries)));
    }

    private static void throwException(TemplateObject node, TemplateObject parent, String reason)
        throws InvalidInputException {
        // Point at the enclosing directive when the offending expression is one of its parameters,
        // so that the reported position matches what the template author wrote.
        TemplateObject target = parent != null && ASTAccessor.isAssignment(parent) ? parent : node;
        throw new InvalidInputException(String.format(
            "Error parsing Freemarker template: invalid input at line %s, column %s: %s",
            target.getBeginLine(), target.getBeginColumn(), reason));
    }

    /**
     * The identifiers a template may reference at a given point in the tree.
     * <p>
     * A name is admitted when it is a root model ({@code data}, {@code entity}), a name the
     * template declares in its namespace ({@code <#assign>}, {@code <#global>}, {@code <#local>}),
     * or a block-local name: a loop variable, a macro parameter or an escape placeholder.
     * <p>
     * Block-local names live in a set that is copied when the validator descends into a body, so
     * they fall out of scope again on the way back up. Namespace and macro names live in sets
     * shared by the whole template, mirroring FreeMarker's template namespace.
     */
    private static final class Scope {

        private final Set<String> locals;
        private final Set<String> namespace;
        private final Set<String> macros;

        private Scope(Set<String> locals, Set<String> namespace, Set<String> macros) {
            this.locals = locals;
            this.namespace = namespace;
            this.macros = macros;
        }

        static Scope forTemplate(Set<String> macroNames, Set<String> namespaceNames) {
            return new Scope(new HashSet<>(ROOT_MODELS), namespaceNames, macroNames);
        }

        /**
         * Returns the scope for the body of a directive: block-local names on top of this scope,
         * with the loop variables also contributing the {@code _index} and {@code _has_next}
         * helpers FreeMarker defines alongside them.
         */
        Scope forBody(Set<String> localNames, Set<String> loopVariables) {
            if (localNames.isEmpty() && loopVariables.isEmpty()) {
                return this;
            }
            Set<String> bodyLocals = new HashSet<>(locals);
            bodyLocals.addAll(localNames);
            for (String loopVariable : loopVariables) {
                bodyLocals.add(loopVariable);
                for (String suffix : LOOP_HELPER_SUFFIXES) {
                    bodyLocals.add(loopVariable + suffix);
                }
            }
            return new Scope(bodyLocals, namespace, macros);
        }

        boolean declares(String name) {
            return locals.contains(name) || namespace.contains(name);
        }

        boolean declaresMacro(String name) {
            return macros.contains(name);
        }
    }
}

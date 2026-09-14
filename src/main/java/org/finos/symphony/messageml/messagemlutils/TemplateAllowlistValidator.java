package org.finos.symphony.messageml.messagemlutils;

import freemarker.core.ASTAccessor;
import freemarker.core.ASTAccessor.ParamKind;
import freemarker.core.TemplateElement;
import freemarker.core.TemplateObject;
import freemarker.template.Template;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.util.Arrays;
import java.util.Collection;
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
 * <li>Disallowing special variables, and method calls other than parameterized built-ins.</li>
 * <li>Blocking numerical interpolation which can be leveraged for SSTI.</li>
 * <li>Enforcing that all variables reference allowed models (e.g., "data" or "entity").</li>
 * <li>Restricting loop and assignment scopes to valid models.</li>
 * <li>Resolving {@code <@.../>} calls against macros actually defined in the same template.</li>
 * </ul>
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
        "SwitchBlock",
        "Case",
        "Assignment",
        "BlockAssignment",
        "Comment",
        "Macro",
        "UnifiedCall",
        "BodyInstruction",
        "ReturnInstruction"
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

    private static final Set<String> ALLOWED_BUILT_INS = allowlistOf(
        "has_content", "size", "length", "index", "keys", "values", "first", "last", "join", "sort_by",
        "upper_case", "lower_case", "cap_first", "trim", "replace", "split", "contains", "starts_with", "ends_with",
        "number", "string", "c", "date", "datetime", "default", "if_exists", "round", "abs",
        "counter", "has_next", "is_first", "is_last", "is_even_item", "is_odd_item", "item_parity", "item_cycle"
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
        validateNode(root, Scope.forTemplate(collectMacroNames(root)), root);
    }

    private static void validateNode(TemplateObject node, Scope scope, TemplateObject parent)
        throws InvalidInputException {
        if (node == null) {
            return;
        }

        rejectUnsafeConstructs(node, parent);

        if (node instanceof TemplateElement) {
            TemplateElement element = (TemplateElement) node;
            if (!ALLOWED_ELEMENT_CLASSES.contains(element.getClass().getSimpleName())) {
                throwException(element, element,
                    "Directive or element " + ASTAccessor.getNodeTypeSymbol(element) + " is not allowed");
            }
            // <#function> shares the <#macro> AST class, so the class allowlist cannot tell them apart.
            if (ASTAccessor.isFunctionDefinition(element)) {
                throwException(element, element, "Directive <#function> is not allowed");
            }
            // An element's parameters are evaluated in the enclosing scope; the names they declare
            // are visible to its body only.
            Scope bodyScope = validateParameters(element, scope, element);
            if (ASTAccessor.isInterpolation(element)) {
                requireModelReference(element, scope);
            }
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
     */
    private static void rejectUnsafeConstructs(TemplateObject node, TemplateObject parent)
        throws InvalidInputException {
        if (ASTAccessor.isSpecialVariable(node)) {
            throwException(node, parent,
                "Special variables like ." + ASTAccessor.getNodeTypeSymbol(node) + " are not allowed");
        }
        if (ASTAccessor.isNumericalOutput(node)) {
            throwException(node, parent, "Numeric interpolation #{...} is not allowed");
        }
        if (ASTAccessor.isMethodCall(node) && !invokesBuiltIn(node)) {
            throwException(node, parent, "Method calls are not allowed");
        }
    }

    private static boolean invokesBuiltIn(TemplateObject methodCall) {
        Object callee = ASTAccessor.getParameterCount(methodCall) > 0
            ? ASTAccessor.getParameterValue(methodCall, 0)
            : null;
        return callee instanceof TemplateObject && ASTAccessor.isBuiltIn((TemplateObject) callee);
    }

    private static void validateExpressionNode(TemplateObject node, Scope scope, TemplateObject parent)
        throws InvalidInputException {
        String builtInKey = ASTAccessor.getBuiltInKey(node);
        if (builtInKey != null) {
            if (!ALLOWED_BUILT_INS.contains(builtInKey)) {
                throwException(node, parent, "Built-in ?" + builtInKey + " is not allowed");
            }
        } else if (!ALLOWED_EXPRESSION_CLASSES.contains(node.getClass().getSimpleName())) {
            throwException(node, parent, "Expression type " + node.getClass().getSimpleName() + " is not allowed");
        }

        if (ASTAccessor.isIdentifier(node)) {
            String name = ASTAccessor.getNodeTypeSymbol(node);
            if (!scope.declares(name)) {
                throwException(node, parent, "Unknown top-level identifier " + name + " is not allowed");
            }
        }
    }

    /**
     * Validates every parameter of {@code node} and returns the scope its body runs in.
     * <p>
     * Names are collected before being admitted, because a parameter may declare a name at a lower
     * index than the source expression it is bound to (an {@code <#assign>} names its target first
     * and its value last).
     */
    private static Scope validateParameters(TemplateObject node, Scope scope, TemplateObject parent)
        throws InvalidInputException {
        boolean bindsOwnSource = false;
        boolean sourceFromModel = false;
        Set<String> loopVariables = new LinkedHashSet<>();
        Set<String> localNames = new LinkedHashSet<>();
        Set<String> namespaceNames = new LinkedHashSet<>();

        int parameterCount = ASTAccessor.getParameterCount(node);
        for (int i = 0; i < parameterCount; i++) {
            Object value = ASTAccessor.getParameterValue(node, i);
            switch (ASTAccessor.getParameterKind(node, i)) {
                case SOURCE_EXPRESSION:
                    validateParameterValue(value, scope, parent);
                    bindsOwnSource = true;
                    sourceFromModel = derivesFromModel(value, scope);
                    break;
                case CALLEE:
                    requireKnownMacro(value, scope, node, parent);
                    break;
                case LOOP_VARIABLE:
                    addName(loopVariables, value);
                    break;
                case LOCAL_NAME:
                    addName(localNames, value);
                    break;
                case NAMESPACE_NAME:
                    addName(namespaceNames, value);
                    break;
                case MACRO_NAME:
                    // Already registered by collectMacroNames; it is not a variable.
                    break;
                default:
                    validateParameterValue(value, scope, parent);
            }
        }

        // <#assign> targets outlive the directive: like FreeMarker's own template namespace they
        // stay visible to everything parsed after it, not just to a body. A target is only admitted
        // when this very directive binds it to model data, so the block form <#assign x>...</#assign>,
        // whose value is rendered content of unknown provenance, never declares anything.
        if (bindsOwnSource && sourceFromModel) {
            scope.declareInNamespace(namespaceNames);
        }

        // <#items as x> binds no source of its own: it iterates the sequence of the enclosing <#list>.
        boolean iteratesModelData = bindsOwnSource ? sourceFromModel : scope.iteratesModelData();
        return scope.forBody(localNames,
            iteratesModelData ? loopVariables : Collections.<String>emptySet(), iteratesModelData);
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
     * Requires that an interpolation renders model data. This keeps {@code ${...}} from emitting
     * values a template computed purely from literals, such as {@code ${7*7}}.
     */
    private static void requireModelReference(TemplateObject interpolation, Scope scope)
        throws InvalidInputException {
        int parameterCount = ASTAccessor.getParameterCount(interpolation);
        for (int i = 0; i < parameterCount; i++) {
            Object value = ASTAccessor.getParameterValue(interpolation, i);
            if (value instanceof TemplateObject && !derivesFromModel(value, scope)) {
                throwException(interpolation, interpolation,
                    "Interpolation expression must reference a model variable (e.g. data or entity)");
            }
        }
    }

    /** True if {@code value} reads at least one identifier that is in scope. */
    private static boolean derivesFromModel(Object value, Scope scope) {
        if (value instanceof List) {
            for (Object item : (List<?>) value) {
                if (derivesFromModel(item, scope)) {
                    return true;
                }
            }
            return false;
        }
        if (!(value instanceof TemplateObject)) {
            return false;
        }

        TemplateObject node = (TemplateObject) value;
        if (ASTAccessor.isIdentifier(node)) {
            return scope.declares(ASTAccessor.getNodeTypeSymbol(node));
        }
        int parameterCount = ASTAccessor.getParameterCount(node);
        for (int i = 0; i < parameterCount; i++) {
            if (derivesFromModel(ASTAccessor.getParameterValue(node, i), scope)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Collects the names of every {@code <#macro>} in the template. FreeMarker resolves macro names
     * against the template namespace, so a {@code <@name/>} call may legitimately precede the
     * definition it refers to; the names have to be known before the tree is walked.
     */
    private static Set<String> collectMacroNames(TemplateElement root) {
        Set<String> names = new HashSet<>();
        collectMacroNames(root, names);
        return names;
    }

    private static void collectMacroNames(TemplateElement element, Set<String> names) {
        int parameterCount = ASTAccessor.getParameterCount(element);
        for (int i = 0; i < parameterCount; i++) {
            if (ASTAccessor.getParameterKind(element, i) == ParamKind.MACRO_NAME) {
                addName(names, ASTAccessor.getParameterValue(element, i));
            }
        }
        for (int i = 0; i < ASTAccessor.getChildCount(element); i++) {
            collectMacroNames(ASTAccessor.getChild(element, i), names);
        }
    }

    private static void addName(Set<String> names, Object value) {
        if (value instanceof String) {
            names.add((String) value);
        }
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
            "Error parsing Freemarker template: invalid input at line %s, column %s",
            target.getBeginLine(), target.getBeginColumn()));
    }

    /**
     * The identifiers a template may reference at a given point in the tree.
     * <p>
     * A name is admitted only once it is known to carry model data: either it is a root model
     * ({@code data}, {@code entity}), or it was declared from an expression that reads one, or it
     * is a formal parameter whose call-site arguments are validated independently in the caller's
     * own scope. Names bound to values a template built purely from literals are deliberately never
     * admitted, which is what confines interpolation to rendering model data.
     * <p>
     * Block-local names — loop variables and macro parameters — live in a set that is copied when
     * the validator descends into a body, so they fall out of scope again on the way back up.
     * {@code <#assign>} targets and macro names instead live in sets shared by the whole template,
     * mirroring FreeMarker's template namespace.
     */
    private static final class Scope {

        private final Set<String> locals;
        private final Set<String> namespace;
        private final Set<String> macros;
        private final boolean iteratesModelData;

        private Scope(Set<String> locals, Set<String> namespace, Set<String> macros, boolean iteratesModelData) {
            this.locals = locals;
            this.namespace = namespace;
            this.macros = macros;
            this.iteratesModelData = iteratesModelData;
        }

        static Scope forTemplate(Set<String> macroNames) {
            return new Scope(new HashSet<>(ROOT_MODELS), new HashSet<String>(), macroNames, false);
        }

        /**
         * Returns the scope for the body of a directive: block-local names on top of this scope,
         * with the loop variables also contributing the {@code _index} and {@code _has_next}
         * helpers FreeMarker defines alongside them.
         */
        Scope forBody(Set<String> localNames, Set<String> loopVariables, boolean iteratesModelData) {
            if (localNames.isEmpty() && loopVariables.isEmpty() && iteratesModelData == this.iteratesModelData) {
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
            return new Scope(bodyLocals, namespace, macros, iteratesModelData);
        }

        void declareInNamespace(Collection<String> names) {
            namespace.addAll(names);
        }

        boolean declares(String name) {
            return locals.contains(name) || namespace.contains(name);
        }

        boolean declaresMacro(String name) {
            return macros.contains(name);
        }

        /** True when the enclosing {@code <#list>} iterates model data, for a nested {@code <#items>}. */
        boolean iteratesModelData() {
            return iteratesModelData;
        }
    }
}

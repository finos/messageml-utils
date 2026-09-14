package freemarker.core;

/**
 * Utility class placed in the {@code freemarker.core} package to bypass package-private
 * access restrictions in FreeMarker's Abstract Syntax Tree (AST) classes.
 * <p>
 * This is necessary for the {@link org.finos.symphony.messageml.messagemlutils.TemplateAllowlistValidator}
 * to introspect the parsed template and enforce security policies (preventing SSTI) before execution.
 * <p>
 * All FreeMarker-specific knowledge belongs here: the validator is written purely against the
 * vocabulary exposed by this class ({@link ParamKind} and the {@code is*} predicates), so that it
 * never has to branch on FreeMarker's internal AST class names.
 */
public class ASTAccessor {

    /**
     * What an AST node's parameter means to the validator.
     * <p>
     * FreeMarker already labels every parameter of every AST node with a {@link ParameterRole};
     * this enum collapses those roles into the handful of cases the validator treats differently,
     * so that scope tracking is driven by the parser's own metadata rather than by hard-coded
     * per-directive parameter positions.
     */
    public enum ParamKind {
        /**
         * A loop variable introduced by {@code <#list>}, {@code <#foreach>} or {@code <#items>};
         * FreeMarker also defines {@code <name>_index} and {@code <name>_has_next} alongside it.
         */
        LOOP_VARIABLE,
        /**
         * A name visible only inside the node's body, bound to an expression supplied by the
         * caller: a {@code <#macro>} parameter, its catch-all parameter, or a {@code <@... ; x>}
         * loop-body parameter.
         */
        LOCAL_NAME,
        /** A name added to the template namespace, such as an {@code <#assign>} target. */
        NAMESPACE_NAME,
        /** The name declared by a {@code <#macro>} definition. */
        MACRO_NAME,
        /**
         * The expression the node's declared names are bound to: the sequence a {@code <#list>}
         * iterates, or the value an {@code <#assign>} stores.
         */
        SOURCE_EXPRESSION,
        /** The macro a {@code <@.../>} call invokes. */
        CALLEE,
        /** Anything else: a sub-expression to validate, or inert compile-time metadata. */
        EXPRESSION
    }

    public static String getNodeTypeSymbol(TemplateObject obj) {
        return obj.getNodeTypeSymbol();
    }

    public static int getParameterCount(TemplateObject obj) {
        return obj.getParameterCount();
    }

    public static Object getParameterValue(TemplateObject obj, int index) {
        return obj.getParameterValue(index);
    }

    /**
     * Classifies the parameter at {@code index} of {@code obj}.
     * <p>
     * Unrecognised roles fall back to {@link ParamKind#EXPRESSION}, which is the fail-safe answer:
     * the validator then walks the parameter and rejects it unless it is an allowlisted expression.
     */
    public static ParamKind getParameterKind(TemplateObject obj, int index) {
        // A macro's own name is its "assignment target"; it lives in the macro namespace, not the
        // variable namespace, so it has to be told apart from an <#assign> target.
        if (obj instanceof Macro && index == 0) {
            return ParamKind.MACRO_NAME;
        }

        ParameterRole role;
        try {
            role = obj.getParameterRole(index);
        } catch (RuntimeException e) {
            return ParamKind.EXPRESSION;
        }

        if (role == ParameterRole.TARGET_LOOP_VARIABLE) {
            // On a <@.../> call this is a loop-body parameter, bound by the macro's <#nested>
            // rather than by an iterated sequence, so it gets no _index/_has_next helpers.
            return obj instanceof UnifiedCall ? ParamKind.LOCAL_NAME : ParamKind.LOOP_VARIABLE;
        }
        if (role == ParameterRole.PARAMETER_NAME || role == ParameterRole.CATCH_ALL_PARAMETER_NAME) {
            return ParamKind.LOCAL_NAME;
        }
        if (role == ParameterRole.ASSIGNMENT_TARGET) {
            return ParamKind.NAMESPACE_NAME;
        }
        if (role == ParameterRole.LIST_SOURCE || role == ParameterRole.ASSIGNMENT_SOURCE) {
            return ParamKind.SOURCE_EXPRESSION;
        }
        if (role == ParameterRole.CALLEE && obj instanceof UnifiedCall) {
            return ParamKind.CALLEE;
        }
        return ParamKind.EXPRESSION;
    }

    public static int getChildCount(TemplateElement element) {
        return element.getChildCount();
    }

    public static TemplateElement getChild(TemplateElement element, int index) {
        return element.getChild(index);
    }

    public static String getBuiltInKey(TemplateObject obj) {
        if (obj instanceof BuiltIn) {
            return ((BuiltIn) obj).key;
        }
        return null;
    }

    public static boolean isSpecialVariable(TemplateObject obj) {
        return obj instanceof BuiltinVariable;
    }

    public static boolean isMethodCall(TemplateObject obj) {
        return obj instanceof MethodCall;
    }

    public static boolean isBuiltIn(TemplateObject obj) {
        return obj instanceof BuiltIn;
    }

    /** True for a {@code ${...}} interpolation. */
    public static boolean isInterpolation(TemplateObject obj) {
        return obj instanceof DollarVariable;
    }

    /** True for a {@code #{...}} numerical interpolation. */
    public static boolean isNumericalOutput(TemplateObject obj) {
        return obj instanceof NumericalOutput;
    }

    /**
     * True for a {@code <#function>} definition, which FreeMarker parses into the same AST class as
     * a {@code <#macro>} but which this validator does not allow.
     */
    public static boolean isFunctionDefinition(TemplateObject obj) {
        return obj instanceof Macro && ((Macro) obj).isFunction();
    }

    /** True for an {@code <#assign>}, {@code <#global>} or {@code <#local>} directive. */
    public static boolean isAssignment(TemplateObject obj) {
        return obj instanceof Assignment;
    }

    /** True for a reference to a top-level variable, i.e. the root of a {@code data.x.y} chain. */
    public static boolean isIdentifier(TemplateObject obj) {
        return obj instanceof Identifier;
    }
}

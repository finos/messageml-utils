package freemarker.core;

/**
 * Utility class placed in the {@code freemarker.core} package to bypass package-private 
 * access restrictions in FreeMarker's Abstract Syntax Tree (AST) classes.
 * <p>
 * This is necessary for the {@link org.finos.symphony.messageml.messagemlutils.TemplateAllowlistValidator} 
 * to introspect the parsed template and enforce security policies (preventing SSTI) before execution.
 */
public class ASTAccessor {
    public static String getNodeTypeSymbol(TemplateObject obj) {
        return obj.getNodeTypeSymbol();
    }

    public static int getParameterCount(TemplateObject obj) {
        return obj.getParameterCount();
    }

    public static Object getParameterValue(TemplateObject obj, int index) {
        return obj.getParameterValue(index);
    }

    public static ParameterRole getParameterRole(TemplateObject obj, int index) {
        return obj.getParameterRole(index);
    }

    public static String getParameterRoleName(TemplateObject obj, int index) {
        ParameterRole role = obj.getParameterRole(index);
        return role != null ? role.toString() : "null";
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

    public static boolean isNumericalOutput(TemplateObject obj) {
        return obj instanceof NumericalOutput;
    }
}

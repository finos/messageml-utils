package org.finos.symphony.messageml.messagemlutils;

import freemarker.core.ASTAccessor;
import freemarker.core.TemplateElement;
import freemarker.core.TemplateObject;
import freemarker.template.Template;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TemplateAllowlistValidator {

    private static final Set ALLOWED_ELEMENT_CLASSES = new HashSet();
    static {
        ALLOWED_ELEMENT_CLASSES.addAll(Arrays.asList(
            "TextBlock",
            "DollarVariable",
            "MixedContent",
            "ConditionalBlock",
            "IfBlock",
            "IteratorBlock",
            "ElseOfList",
            "ListElseContainer",
            "Sep",
            "BreakInstruction",
            "SwitchBlock",
            "Case",
            "Assignment",
            "BlockAssignment",
            "Comment"
        ));
    }

    private static final Set ALLOWED_EXPRESSION_CLASSES = new HashSet();
    static {
        ALLOWED_EXPRESSION_CLASSES.addAll(Arrays.asList(
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
            "UnaryPlusMinusExpression"
        ));
    }

    private static final Set ALLOWED_BUILT_INS = new HashSet();
    static {
        ALLOWED_BUILT_INS.addAll(Arrays.asList(
            "has_content", "size", "length", "index", "keys", "values", "first", "last", "join", "sort_by",
            "upper_case", "lower_case", "cap_first", "trim", "replace", "split", "contains", "starts_with", "ends_with",
            "number", "string", "c", "date", "datetime", "default", "if_exists", "round", "abs"
        ));
    }

    public static void validate(Template template) throws InvalidInputException {
        TemplateElement root = template.getRootTreeNode();
        if (root == null) {
            return;
        }

        List inScopeVars = new ArrayList();
        inScopeVars.add("data");
        inScopeVars.add("entity");
        validateNode(root, inScopeVars, root);
    }

    private static void validateNode(TemplateObject obj, List inScopeVars, TemplateObject parent) throws InvalidInputException {
        if (obj == null) {
            return;
        }

        String className = obj.getClass().getSimpleName();

        if (ASTAccessor.isSpecialVariable(obj)) {
            throwException(obj, parent, "Special variables like ." + ASTAccessor.getNodeTypeSymbol(obj) + " are not allowed");
        }

        if (ASTAccessor.isMethodCall(obj)) {
            throwException(obj, parent, "Method calls are not allowed");
        }

        if (ASTAccessor.isNumericalOutput(obj)) {
            throwException(obj, parent, "Numeric interpolation #{...} is not allowed");
        }

        if (obj instanceof TemplateElement) {
            parent = obj; // Use the current structural element as the parent for its children/expressions
            if (!ALLOWED_ELEMENT_CLASSES.contains(className)) {
                throwException(obj, parent, "Directive or element " + ASTAccessor.getNodeTypeSymbol(obj) + " is not allowed");
            }

            if (className.equals("IteratorBlock")) {
                int paramCount = ASTAccessor.getParameterCount(obj);
                if (paramCount >= 2) {
                    Object source = ASTAccessor.getParameterValue(obj, 0);
                    Object targetVar = ASTAccessor.getParameterValue(obj, 1);
                    if (source instanceof TemplateObject) {
                        validateNode((TemplateObject) source, inScopeVars, parent);
                    }
                    
                    List childScope = new ArrayList();
                    childScope.addAll(inScopeVars);
                    if (targetVar instanceof String && source instanceof TemplateObject) {
                        if (referencesModel((TemplateObject) source, inScopeVars)) {
                            childScope.add((String) targetVar);
                        }
                    }
                    
                    TemplateElement element = (TemplateElement) obj;
                    for (int i = 0; i < element.getChildCount(); i++) {
                        validateNode((TemplateObject) element.getChildAt(i), childScope, parent);
                    }
                    
                    for (int i = 2; i < paramCount; i++) {
                        Object param = ASTAccessor.getParameterValue(obj, i);
                        validateParam(param, inScopeVars, parent);
                    }
                    return;
                }
            } else if (className.equals("Assignment") || className.equals("BlockAssignment")) {
                int paramCount = ASTAccessor.getParameterCount(obj);
                if (paramCount >= 2) {
                    Object varName = ASTAccessor.getParameterValue(obj, 0);
                    Object expr = ASTAccessor.getParameterValue(obj, 1);
                    if (expr instanceof TemplateObject) {
                        validateNode((TemplateObject) expr, inScopeVars, parent);
                    }
                    
                    if (varName instanceof String && expr instanceof TemplateObject) {
                        if (referencesModel((TemplateObject) expr, inScopeVars)) {
                            inScopeVars.add((String) varName);
                        }
                    }
                    
                    for (int i = 2; i < paramCount; i++) {
                        Object param = ASTAccessor.getParameterValue(obj, i);
                        validateParam(param, inScopeVars, parent);
                    }
                    return;
                }
            } else if (className.equals("DollarVariable")) {
                int paramCount = ASTAccessor.getParameterCount(obj);
                if (paramCount >= 1) {
                    Object content = ASTAccessor.getParameterValue(obj, 0);
                    if (content instanceof TemplateObject) {
                        validateNode((TemplateObject) content, inScopeVars, parent);
                        if (!referencesModel((TemplateObject) content, inScopeVars)) {
                            throwException(obj, parent, "Interpolation expression must reference a model variable (e.g. data or entity)");
                        }
                    }
                }
                return;
            }

            TemplateElement element = (TemplateElement) obj;
            for (int i = 0; i < element.getChildCount(); i++) {
                validateNode((TemplateObject) element.getChildAt(i), inScopeVars, parent);
            }
        } else {
            String biKey = ASTAccessor.getBuiltInKey(obj);
            if (biKey != null) {
                if (!ALLOWED_BUILT_INS.contains(biKey)) {
                    throwException(obj, parent, "Built-in ?" + biKey + " is not allowed");
                }
            } else {
                if (!ALLOWED_EXPRESSION_CLASSES.contains(className)) {
                    throwException(obj, parent, "Expression type " + className + " is not allowed");
                }
            }

            if (className.equals("Identifier")) {
                String name = ASTAccessor.getNodeTypeSymbol(obj);
                if (!inScopeVars.contains(name)) {
                    throwException(obj, parent, "Unknown top-level identifier " + name + " is not allowed");
                }
            }
        }

        int paramCount = ASTAccessor.getParameterCount(obj);
        for (int i = 0; i < paramCount; i++) {
            Object param = ASTAccessor.getParameterValue(obj, i);
            validateParam(param, inScopeVars, parent);
        }
    }

    private static void validateParam(Object param, List inScopeVars, TemplateObject parent) throws InvalidInputException {
        if (param instanceof TemplateObject) {
            validateNode((TemplateObject) param, inScopeVars, parent);
        } else if (param instanceof List) {
            for (Object item : (List) param) {
                if (item instanceof TemplateObject) {
                    validateNode((TemplateObject) item, inScopeVars, parent);
                }
            }
        }
    }

    private static boolean referencesModel(TemplateObject obj, List inScopeVars) {
        if (obj == null) {
            return false;
        }

        String className = obj.getClass().getSimpleName();
        if (className.equals("Identifier")) {
            String name = ASTAccessor.getNodeTypeSymbol(obj);
            return inScopeVars.contains(name);
        }

        int paramCount = ASTAccessor.getParameterCount(obj);
        for (int i = 0; i < paramCount; i++) {
            Object param = ASTAccessor.getParameterValue(obj, i);
            if (param instanceof TemplateObject) {
                if (referencesModel((TemplateObject) param, inScopeVars)) {
                    return true;
                }
            } else if (param instanceof List) {
                for (Object item : (List) param) {
                    if (item instanceof TemplateObject) {
                        if (referencesModel((TemplateObject) item, inScopeVars)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    private static void throwException(TemplateObject obj, TemplateObject parent, String reason) throws InvalidInputException {
        TemplateObject target = obj;
        if (parent != null && parent.getClass().getSimpleName().equals("Assignment")) {
            target = parent;
        }
        throw new InvalidInputException(String.format("Error parsing Freemarker template: invalid input at line %s, column %s",
                target.getBeginLine(), target.getBeginColumn()));
    }
}

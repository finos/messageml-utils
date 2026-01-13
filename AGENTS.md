# AI Coding Agent Context

This document provides context for AI coding assistants to understand the `messageml-utils` project.

## Project Overview

`messageml-utils` is a Java library for parsing, processing, and rendering messages written in Symphony's MessageML format. It can also handle messages in Markdown and convert between formats. The library supports Freemarker templates for dynamic message generation, using data provided in a format called EntityJSON.

The core of the library consists of:
- A MessageML parser (`MessageMLParser`) that tokenizes and parses MessageML into a document tree.
- A Markdown parser (`MarkdownParser`) for handling Markdown input.
- An EntityJSON parser (`EntityJsonParser`) for validating and processing structured data used in messages.
- `MessageMLContext` which is the main entry point for using the library.

The project is built with Apache Maven and targets Java 1.8.

## Project Structure

```
src/
├── main/java/org/finos/symphony/messageml/messagemlutils/
│   ├── MessageMLContext.java    # Main entry point
│   ├── MessageMLParser.java     # Core MessageML parser
│   ├── bi/                      # Business Intelligence components
│   ├── elements/                # MessageML element implementations
│   ├── exceptions/              # Custom exceptions
│   ├── markdown/                # Markdown parsing/rendering
│   └── util/                    # Utility classes
├── main/resources/
│   └── emoji.properties         # Emoji mappings
└── test/
    ├── java/                    # Unit tests (JUnit 5)
    └── resources/               # Test fixtures and payloads
```

## Building and Running

The project is built and tested using Maven.

- **To build the project and run tests:**
  ```bash
  ./mvnw -B --file pom.xml verify
  ```
  This command will compile the source code, run the unit tests, and package the library into a JAR file.

- **To clean the project:**
  ```bash
  ./mvnw clean
  ```

- **To run a single test class:**
  ```bash
  ./mvnw -B test -Dtest=ClassName
  ```

- **To run a single test method:**
  ```bash
  ./mvnw -B test -Dtest=ClassName#methodName
  ```

## Development Conventions

- **Code Style:** The project uses the `.editorconfig` file to define basic coding style settings.
- **Testing:** The project uses JUnit 5 for unit testing. Tests are located in the `src/test/java` directory. New features should be accompanied by corresponding unit tests.
- **Dependencies:** Project dependencies are managed in the `pom.xml` file. Key libraries include Jackson for JSON processing, `com.github.fge.jsonschema` for JSON schema validation, and Freemarker for templating.
- **Continuous Integration:** The project uses GitHub Actions for CI. The workflow is defined in `.github/workflows/build.yml`.
- **Contributing:** Contribution guidelines can be found in `.github/CONTRIBUTING.md`.

## Key Concepts

### MessageML
A markup language superset of PresentationML, adding convenience tags for complex or frequently used constructs. It is translated to PresentationML internally by the Symphony Agent API.

### PresentationML
A strict subset of HTML5 describing the markup which controls the formatting (presentation) of a message. This is the format used internally by Symphony.

### EntityJSON
JSON data representing complex financial objects and other structured data. EntityJSON elements are referenced in PresentationML.

### Freemarker Templates
Templating language providing macros (conditional blocks, iterations, assignments) to expand message templates into MessageML messages.

## Usage Example

```java
/* Instantiate the parsing context */
MessageMLContext context = new MessageMLContext(dataProvider);

/* Parse the message and entity data */
context.parseMessageML(message, entityJSON, version);

/* Get various representations */
Element messageML = context.getMessageML();      // Document tree
String presentationML = context.getPresentationML(); // PresentationML output
String entityJSON = context.getEntityJson();     // Entity data
String markdown = context.getMarkdown();         // Markdown output
String text = context.getText();                 // Plain text
```

## License

Apache License, Version 2.0

[![Licence](https://img.shields.io/badge/licence-Apache%20Licence%20%282.0%29-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![Maven Central](https://img.shields.io/maven-central/v/org.symphonyoss.symphony/messageml.svg)](http://search.maven.org/#search%7Cga%7C1%7Ca%3A%22messageml%22)
[![Build Status](https://travis-ci.org/symphonyoss/messageml-utils.svg)](https://travis-ci.org/symphonyoss/messageml-utils)
[![Validation Status](https://scan.coverity.com/projects/12785/badge.svg?flat=1)](https://scan.coverity.com/projects/symphonyoss-messageml-utils)
[![Test Coverage](https://codeclimate.com/github/symphonyoss/messageml-utils/badges/coverage.svg)](https://codeclimate.com/github/symphonyoss/messageml-utils/coverage)

# Introduction

MessageML is a markup language used by the Symphony Agent API for representing messages, including formatting (bold, italic, numbered and unnumbered lists etc.) 
and entity data representing [_structured objects_](https://rest-api.symphony.com/docs/objects). 

The format is intended to allow third parties to create and render rich content messages representing complex objects, 
enabling deep workflow integrations across multiple systems, with Symphony being the central hub.

# Description

Support of Symphony structured objects comprises the ability to parse and render the following formats:

* [MessageML](https://rest-api.symphony.com/docs/messagemlv2), 
a superset of PresentationML, adding a number of convenience tags for more complex or frequently used constructs.
It is translated to PresentationML internally by the Agent API before ingestion in Symphony.
* [PresentationML](https://rest-api.symphony.com/docs/messagemlv2#reading-messageml-messages), 
a strict subset of HTML5 describing the markup which controls the formatting (presentation) of a message. 
PresenttionML is the format used internally by Symphony and returned from message ingestion and retrieval by the Agent API.
* [EntityJSON](https://rest-api.symphony.com/docs/objects#message-and-object-presentation), 
JSON data representing complex financial objects and other structured data. 
EntityJSON elements are referenced in PresentationML and are provided by the message originator 
or generated automatically during the expansion of MessageML convenience tags.
* [Freemarker](http://freemarker.org/),
a templating language providing macros (conditional blocks, iterations, assignments etc.) 
to expand message templates into MessageML messages. Freemarker support is experimental.
* [Markdown](http://daringfireball.net/projects/markdown/syntax),
the legacy text format used by Symphony. 
Markdown parsing and rendering capabilities are provided for compatibility with older versions of Symphony.

More specifically, the MessageML parser can be used to:

* validate the input message as correct MessageML or PresentationML and the input data as correct EntityJSON; 
if the input message contains references to entity data, the validation process verifies if the provided data matches those references 
* expand Freemarker templates into MessageML messages; this includes processing Freemarker macros 
and injecting EntityJSON data referenced by template variables
* construct a document tree representing the message
* serialize the MessageML tree as PresentationML; this step includes generation of EntityJSON data from convenience MessageML tags
* serialize the MessageML tree as Markdown
* parse Markdown input into a MessageML document tree

# Usage

```java
/* Instantiate the parsing context. The "dataProvider" object is used to resolve user mentions and check supplied URLs against a whitelist of supported URI schemes. */
MessageMLContext context = new MessageMLContext(/*IDataProvider*/ dataProvider);

/* Parse the message and entity data */
context.parseMessageML(/*String*/ message, /*String*/ entityJSON, /*String*/ version);

/* Parse a Markdown message into a MessageML document tree */
context.parseMarkdown(/*String*/ markdown, /*JsonNode*/ entities, /*JsonNode*/ media);

/* Get the MessageML document tree */
Element messageML = context.getMessageML();

/* Get the PresenttionML representation of the message */
String presentationML = context.getPresentationML();

/* Get the entity data of the message, including data auto-generated from MessageML convenience tags*/
String entityJSON = context.getEntityJson();

/* Get the Markdown representation of the message */
String markdown = context.getMarkdown();
```

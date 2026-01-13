# Symphony AI Context (`sym-ai-context`)

## Summary

The `<sym-ai-context>` element is a MessageML tag used to provide contextual information to Symphony AI. This element is not rendered in the message but is used to pass a collection of Symphony message, stream, and attachment identifiers to the AI for it to have a context to work with.

## Specification

### MessageML Tag

`<sym-ai-context>`

### EntityJSON Representation

The `<sym-ai-context>` element is converted into a JSON object within the EntityJSON payload.

**Type**: `com.symphony.ai.context`
**Version**: `1.0`

The JSON object contains three arrays: `messages`, `streams`, and `attachments`. Each object in these arrays contains the `id` of the corresponding element.

**Example EntityJSON:**

```json
{
  "sym-ai-context1": {
    "type": "com.symphony.ai.context",
    "version": "1.0",
    "messages": [
      {
        "id": "msg1"
      }
    ],
    "streams": [
      {
        "id": "stream1"
      }
    ],
    "attachments": [
      {
        "id": "file1",
        "streamId": "stream2",
        "messageId": "msg2"
      }
    ]
  }
}
```

### Allowed Children

The `<sym-ai-context>` element can only contain the following child elements:

*   `<message>`: Represents a Symphony message. Requires an `id` attribute.
*   `<stream>`: Represents a Symphony stream. Requires an `id` attribute.
*   `<attachment>`: Represents a file attachment. Requires `streamId`, `messageId`, and `fileId` attributes.

### Constraints

*   The `<sym-ai-context>` element cannot contain any text content.
*   Any other elements besides the allowed children will result in a parsing error.

### PresentationML

In PresentationML, the `<sym-ai-context>` element is rendered as an empty `<span>` tag with a `data-entity-id` attribute pointing to the corresponding EntityJSON object.

**Example PresentationML:**

```xml
<div data-format="PresentationML" data-version="2.0">
  <span class="entity" data-entity-id="sym-ai-context1"></span>
</div>
```

### Example Usage

```xml
<messageML>
  <sym-ai-context>
    <stream id="stream1"/>
    <message id="msg1"/>
    <attachment streamId="stream2" messageId="msg2" fileId="file1"/>
  </sym-ai-context>
  Hello, I am a message with AI context.
</messageML>
```

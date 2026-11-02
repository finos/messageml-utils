One block that mixes two problems at once: FreeMarker-looking syntax and a mismatched tag, plus a
second, separately broken block further down:

<messageML>
<p>Balance due: ${amount} <b>overdue</p>
</messageML>

Some normal Markdown in between: *this part is fine*.

<messageML>
<div style="color:red;">
<span>Unclosed span inside an unclosed div
</messageML>

Two broken blocks, one message, both should degrade independently.

Two separate blocks, both mismatched, with plain Markdown between them:

<messageML>
<card>
<header>First broken card
<body>Missing header close tag.</body>
</card>
</messageML>

That first one should degrade on its own. Here is a **second**, differently broken block:

<messageML>
<p>Open paragraph with <b>bold that never closes
</messageML>

Everything outside the two blocks should stay readable.

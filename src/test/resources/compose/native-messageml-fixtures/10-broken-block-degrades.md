Trying to render a mismatched block on purpose, to check that it degrades instead of breaking the
whole message:

<messageML>
<card accent="tempo-bg-color--blue">
<header>Broken card</header>
<body><b>Unclosed bold and mismatched tags</p>
</card>
</messageML>

Everything above and below this block should still be readable even if the block itself degrades.

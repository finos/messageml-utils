A block written with HTML habits instead of strict MessageML (named entities, unescaped characters,
`<strong>`/`<em>`/`<u>`, a `<br>` with no slash, a comment and an emoji shortcode with colons):

<messageML>
<p><strong>Heads up:</strong> the <em>Q3 P&L</em> report&nbsp;is out &mdash; see <a href="https://example.com/report?q=3&format=pdf">the PDF</a>.<br>
Revenue < forecast by 2%, margin > target.</p>
<!-- internal note: double-check the numbers -->
<p><u>Owner:</u> finance team <emoji shortcode=":bar_chart:"/></p>
</messageML>

All of it should render, with nothing shown as raw source.

A valid block first, to confirm a later broken block does not take it down too:

<messageML>
<card accent="tempo-bg-color--blue">
<header>Valid card</header>
<body>This one is well-formed.</body>
</card>
</messageML>

Now a broken one right after it:

<messageML>
<table>
<thead>
<tr><td>Column A</td><td>Column B</td>
</thead>
<tbody>
<tr><td>Row 1, A</td></tr>
</table>
</messageML>

And a third, valid block again to prove recovery continues after the broken one:

<messageML>
<p>Back to normal, filed under <hash tag="release"/>.</p>
</messageML>

Done.

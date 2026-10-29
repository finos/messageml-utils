A table and a list where the model forgot most of the structural closing tags:

<messageML>
<h3>Open incidents</h3>
<table>
<tr><th>Id</th><th>Status</th>
<tr><td>INC-1</td><td>Investigating
<tr><td>INC-2</td><td>Resolved</td>
</table>
<ul>
<li>Pager rotation updated
<li>Runbook reviewed</li>
<ul><li>Owner: SRE</li></ul>
</ul>
<p>Last refreshed <b>5 minutes ago</p>
</messageML>

The table should have three rows and the "Owner" bullet should be nested under "Runbook reviewed".

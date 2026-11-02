Testing a block that tries to sneak in a form, which is not allowed in a native block:

<messageML>
<form id="broken-form">
<text-field name="feedback" placeholder="Type here"/>
<button name="submit">Submit</button>
</form>
</messageML>

This should degrade rather than render an interactive form.

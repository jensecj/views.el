A simple way to switch between different views of buffers.

`Views.el` tries to restore buffers in their correct location, with their right
sizes, and with the point where you left it. It'll also reopen terminals, and is
easily extendable.

<p align="center">
<img src="screencast.gif?raw=true">
</p>

Save the current view with `(views-push)`, remove a view with `(views-pop)`, and
switch to another view with `(views-switch)`.

Views are saved in `views-file`.

Information about open buffers in a frame is collected by the functions in
`views-collect-functions`. Each function is called with an open buffer as an
argument, and should return a single, or a list of, key-value-pairs which can be
used to restore the properties later.

Restoring properties for a buffer is done by the functions in
`views-restore-functions`, they're called with an alist of collected information
about the buffer, and the buffer in question is current. Restoration functions
are considered side-effecty, and their return values are ignored.

If a buffer is non-existant, `views` will try to ressurect it using the
functions in `views-resurrect-functions`. Each function is called with
information about the dead buffer, and should return a live buffer.

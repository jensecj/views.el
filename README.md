A simple way to switch between different views of buffers.

`Views.el` tries to restore buffers in their correct location, with their right
sizes, and with the point where you left it. It'll also reopen terminals.

<p align="center">
<img src="screencast.gif?raw=true">
</p>

Save the current view with `(views-push)`, remove a view with `(views-pop)`, and
switch to another view with `(views-switch)`.

Views are saved in `views-file`.

To see which information is saved for each window, look at
`views--collect-window-info`.

add some bootstrap theming
-- // expose UtilQQ unsafe
-- Look at Alder data structures
zip together data, layout, theme-- use the simpler types and runtime checking.

add in the state monad.

make tabs
make radio button
make sortof radiobutton
make top level event handler on static node that uses id map
update id map on each state transition
-- handle focus/blur events
handle page load etc events



\section{DOM Input Event Handling}
DOM input events are fired whenever an {\em input} or {\em textarea} form element are modified by the user.  Unlike other events where either the view or model must be updated by the application, the modification of the view is done immediately by the browser, as the event is sent.  Usually then, there is no need to update the display.  With a differencing algorithm in place, this might be recognized, but it seems reasonable to allow for the redraw to be elided.  The handlers for this type of event thus simply omit the call to redraw.

Hmm... I think this is probably the wrong thing.  Assume we have something like a chat window, where you type in an input field and the results are echoed in another part of the screen.  This optimization would break that connection.  So, it must either be optional or it must be handled in the differencing engine.  One possibility would be to represent the DOM changes made by the browser and apply those to the DOM state kept in the differencing engine.  That seems like it would be the cleanest.

see Alderon.lhs

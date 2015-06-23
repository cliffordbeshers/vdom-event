\section{Introduction}

Alderon is a client-side web client library written in Haskell, compiled with ghcjs.

\subsection{History}

ghcjs-dom is well done, but mirrors standard web toolkits.  As such it is large, verbose, and imperative.

ghcjs-jquery is a faithful representation of jquery, but had a bug in
removing event handlers.  By the time I had fixed that, I realized I
was the only one using it and that I didn't want to be using jQuery
any more.  Haskell provides better expressive tools and jQuery still
is stuck in the world of twiddling bits around the DOM for updates.

ghcjs-sodium is a library largely abandoned.  Within it, I found
Alder, which was the nicest looking Haskell web framework I had ever
seen.  Unfortunately, the example program didn't run and it was
designed with animation in mind, side-stepping many of the issues of
dealing with forms in browsers.  But it was an excellent starting
point and I chose to honor that by calling this project Alderon, as in
moving on.  The Star Wars reference came unbidden.

Alder has a 'reconcile' function that updates the DOM.  It is not
amazingly complicated, nor is it amazingly clear.  Assuming that
dealing with the DOM will always be problematic, I looked to gdiff to
provide a general framework.  gdiff only works pure values; the
problem of converting it to work on a stateful DOM remains unsolved.


\section{Control Flow in Alderon}

\subsection{Model and View}

The MVC model is well established and perhaps not so well defined.  I have discovered some concrete criteria that are useful in providing two standard interation metaphors: infinite undo; and browsing history.  Changes to the model are recorded in the undo history, changes to the view in the browsing history.

Thus, both model and view types support serializable operations, as
these values need to get stored in client and server databases, as
well as being shared between users. Alderon's view of one of these

\begin{code}

\end{code}

\subsection{Event loop}

eventLoop :: Show a => DOMElement -> MVar (a -> a) -> (MVar (a -> a) -> a -> Html) -> a -> IO ()
eventLoop root redrawChannel render state = do
  update <- takeMVar redrawChannel
  let state' = update state
  hs <- buildDOM (render redrawChannel state')
  old <- detachChildren root
  appendChildren root hs
  print state'
  eventLoop root redrawChannel render state'


\subsection{DOM Input Event Handling}
DOM input events are fired whenever an {\em input} or {\em textarea} form element are modified by the user.  Unlike other events where either the view or model must be updated by the application, the modification of the view is done immediately by the browser, as the event is sent.  Usually then, there is no need to update the display.  With a differencing algorithm in place, this might be recognized, but it seems reasonable to allow for the redraw to be elided.  The handlers for this type of event thus simply omit the call to redraw.

Hmm... I think this is probably the wrong thing.  Assume we have something like a chat window, where you type in an input field and the results are echoed in another part of the screen.  This optimization would break that connection.  So, it must either be optional or it must be handled in the differencing engine.  One possibility would be to represent the DOM changes made by the browser and apply those to the DOM state kept in the differencing engine.  That seems like it would be the cleanest.


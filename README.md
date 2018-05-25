# The libgtk+ (Gtk+ v2) Back-End Provider for the Happlets Framework

*WARNING:* There are still some known bugs in this package, it is being
uploaded to Hackage for evaluation purposes only. Although it is being
actively developed, there is no release schedule for a production-ready
version of this package. Contributions to the repository on GitHub are
welcome.

This packages provides the Gtk+ v2 back-end to the "happlets" GUI
framework. To create a Gtk+ applet, import the "Happlets.Lib.Gtk"
module. "Happlets.Lib.Gtk" re-exports the "Happlets" module, so you can
use all of the primitives provided by the "Happlets" module to construct
your applet. In your "main" function, launch the main event loop by
passing the 'Happlets.Lib.Gtk.gtkHapplet' function as the "Provider"
either to the 'Happlets.Initialize.happlet' function or to the
'Happlets.Initialize.simpleHapplet' function.

For an example of how to program your own Happlet, run the "cabal
configure" command with the "--enable-tests" flag set. This will build
the "Happlets.Lib.Gtk.TestSuite" executable program. Refer to the source
code for "TestSuite" to see how the application is structured.

The goal of the Happlets project is to allow you to create very simple,
thread-safe applications that contain nothing more than a single window
with a drawing canvas that can respond to user input events, like mouse
clicks, key-presses, or frame animation events. The intention is to
create a minimal programming platform for small, single-purpose
graphical applications which simply displays some interactive graphic,
for example a plot of some data, or a simple game. Naturally, the
Happlet program can be arbitrarily complex, but it may be better to
consider other, FRP-based solutions if managing events becomes too
difficult.

Please refer to the Happlets package on the Hackage website:

https://hackage.haskell.org/package/happlets

## Known Bugs

The implementation for the 'windowChangeHapplet' function in the
'Happlets.GUI.HappletWindow' type class is known to result in the window
freezing.

The 'setPoint' and 'getPoint' functions in the
'Happlets.Draw.Happlet2DGraphics' type class are 'undefined' and will
throw a runtime exception in the GUI event handler thread if
evaluated. Every event handled triggers code execution in it's own
light-weight thread, so this will not crash the whole program, only that
one thread.

The 'Happlets.GUI.CanBufferImages' type class has not been tested at
all, and in fact I am evaluating the possibility of rewriting the
implementation to make use of Cairo "Surfaces" rather than Gtk+
"PixMaps" as the means by which to buffer off-screen images.

Another bug is in the screen refresh. Every time the graphics in the
window change, the entire window is re-blitted, which is very
inefficient. The code for selectively blitting only the changed portion
of the window has not been implemented yet.

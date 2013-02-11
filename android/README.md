# Android Projects

Over my time at Hampshire, I have created any number of android projects.  As I looked through my harddrive, I found some consistency: broken and poor documentation.  I've included the ones that "look the best" from a code standpoint, but it's hard to get at sense if they work based on purusing the source folders.

Feel free to browse the code, here is my recollection of what each is:


## widgets/RadioGrid

An attempt to create radio buttons in a MxN grid instead of just in
a row or col. The purpose was to use in a D&D alignment selection, but
the case seemed general enough to abstract I attempted to make sure
the widget was fully themeable via android's XML styling. As it is, I found
the code almost completely commented out so it probably was abandoned
halfway through debugging or migrated to a different project.


## HeroCritter

One of the larger/more complete attempts at a final project for mobile
computing. It was supposed to be a tamagotchi like game where you played
as a fairy guiding a Hero on their journey. I had pages and pages of UI
sketches and case studies for this one.

Most of the "code" is actually in the `res` folder. I was trying to take
as much advantage of androids layouts/styles/resources to handle the
graphics instead of programmatically.

I got in my own way with this projects, focusing to much on the minutia
of how the interface would behave and look. This combined with my lack
of artistic skill lead to an overly ambitious project that got nowhere.


## TrfsWebView

A quick wrapper for the web prototype of **Troll Run From Sun**. Simply
loads up the existing web page and forwards multitouch events to the
WebView via javascript commands.


## SimpleCounter

I honestly have no idea what this does. Best guess would be a simple way
to tally various items, one item, or maybe a stop watch. In fact, it
might not do anything as it is.

Still, this is a decent example of the kind of thing I make as a way to
test something I just learned. This looks to be from when I was learning
about layouts and preferences management in Android.


## Termites

While teaching myself about RenderScripts (a custom shader language for
android) I decided to program an alife Termite simulation.  This is one of my
goto examples for perpixel simulations when I'm learning a new system; 
it is a bit more than a toy and I'm intimately familiar with how it should
look.  While this example runs, it does not seem to be running absolutely
correctly though this may just be due to how random numbers are used in
renderscripts.

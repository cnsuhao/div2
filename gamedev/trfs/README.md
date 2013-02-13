# Troll Run From Sun

**TRFS** was an infinite platformer where are troll raced away from the
rising sun. The actual game did not make a lot of sense but the single
touch mechanics were smooth. At least until inserted into Cabana.

## Cabana 
Cabana was the worst possible tool for developing this game.
Anything we tried to do in the game was unbearably slow when written
in the native Cabana. So, instead I used the "javascript nodes" and
programmed around their engine as much as possible. It got to the point
where I could tap into Cabana when I wanted to take advantage of their
interface for configuration but leave it alone for the actual logic.

Most of the project was spent working around Cabana in order to get this
realtime twitch game up to speed. Quite often the whole project would
break as the Cabana developers would change some feature at the 11th
hour.

While the actual engine itself was a lot of work with DOM nodes instead
of a canvas, it paid off since we could use stylesheets to animate
various aspects more smoothly than if just drawn. Though in order to get
CSS working, I had to create a mini "JSS" DSL to program CSS styles from
javascript.

### Features: 
 * **JSS** a javascript css dsl
 * **asset preloaded** assets were preloaded from cabana instead of
asynchronously
 * **CSS class animations**
 * **Level Generation** the ability to generate and connect roomtypes.
Got it working but not in time to integrate it in final version.
 * **Infinite Levels** the levels would automatically keep generating
themselves forever using a PRNG
 * **Fluid Movement** with a tap or a downward drag, the troll could
run, jump, dash, cancel, fall, walljump, and roll


## HTML Prototypes

Until I really got a hand on how to use Cabana, most of the games
prototypes were just done in pure HTML.

In the end, the game itself was primarily written *around* Cabana
instead of *in* it, so a lot of these workarounds helped. 


## Android Prototypes
For a bit of time I tried to recreate various aspects of the game on
just Android. It became rapidly clear that although this would be
more challenging, it wouldn't work for the group. Not much was done
beyond getting decent at drawing in either the Android canvas or just a
webview.

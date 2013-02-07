# Coin Get: Multiplayer Madness

Some groups for the Pygame Course wanted to create something with
networking. Unfortunately, this is still incredibly poorly documented
as far as I can find. I knew they could do their own threading or use
twisted but I had never actually attempted this with pygame. So, I
decided to start creating an example. It took me about a week or two of
serious work longer than I expected to get a prototype working, not even
a final project. The final gameplay was buggy and twitched a bit, but
seems to work reasonably well.

In the end, making things networked was such a challenge for me that I
had to recommend that the groups that wanted to try it steer clear for
now; they could always add that feature after the course was over if
they wanted. Still, making a multiplayer python game is something I have
known I could do but I never have done in the past. Doing it more or
less from scratch was just a bonus. And now, to my knowledge, this is
the only pure pygame/twisted example I know of.

## Gameplay
Same as super coin get.

## Features
The features I was attempting to demonstrate by the end of the project included:

 * Pygame/Twisted loop integration
 * pubsub signalling
 * AMP messaging -> signals -> game
 * Local and Networked PlayerControllers 

## Usage
 * Make sure to run `server.py` and then `client.py`.  You can use `--help` to
   get options for either command.

## Requirements
 * python 2.6+
 * pygame
 * twisted
 * louie

![Coin Get: Multiplayer Madness](https://github.com/alecgoebel/div2/raw/master/games/coinget-multi/screens/three_clients.png "Coin Get: Multiplayer Madness")


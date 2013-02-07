To Install:
--------------------------------------------
Simply unzip onto a Windows machine with VB



To Run:
--------------------------------------------
Shortcuts have been provided for your convinience, the main executable is
HCIProject4.exe.  If for whatever reason the shortcut does not work, either the
code itself, opened from the solution, or the actual compile should.  Please
let me know of any issues immediately at agoebel@wpi.edu

User creation works, but if you do not feel like creating one, the user
"sampleuser" (pass: asdf) has been included in both the debug and compiled
version.



Notes on HCIProject4:
---------------------------------------------
The bookcatalog.xml file has been modified from its original form.  If there is
a different catalog you wish to load and it is in the same sample as the
original, convert it with the CatalogConverter tool.  Also, the code which
loads the document is MainState.vb, and the comments within it should make it
clear where to change the file name.

At this point, all functionality is working.  This includes very rigorous error
checking to make sure books aren't added twice to wish lists and that a user is
logged in before going to the registered page.  

Buttons labeled broken do not work and were never intended to.  Broken simply
means it will launch a useless MsgBox such as "order placed" when there was
nothing of the like.

I apologize in advance for the extreme lack of commenting, helper functions,
and the copy/pasting.  Normally I am not this bad but due to the odd syntax and
quirky nature of VB's GUI elements, the bulk of the code was "hacked" out,
going straight from prototype into insertion.

Lastly, the only resources I used were from various tutorial sites and the
course VB Book.  The only code that truly is not mine is Serializable and the
xml reader in CatalogConverter.  The variables were renamed only so I could
learn what was going on.

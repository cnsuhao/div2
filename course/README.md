# Pygame Course

Here is most of the code developed during the preparation and running of the pygame course.


## classcode
As we completed code in class, we tried to upload the more useful days
here so students who didn't have a working copy by the end of class
could find one.

## samplecode
As students worked on their final projects, many got caught in similar
areas (tiles, menus, cameras, etc). As I helped students with these
problems, I made sure to save what we hacked out together as an example.
Then, I cleaned the results a bit and put them up here so that everyone
could use it as a reference. This also helped be more consistent in
teaching students about these higher level game mechanics.


## internal
This folder contains an abridged version of what was cluttlering the "teaching"
repo of the course. This is where Paul, the other TAs, and myself would store
any work in progress code or notes to review in our weekly meetings.


## homeworks
These folders are what the students forked to complete for the homework
assignments.  Towards the middle of the course, I attempted to use TDD to help
them debug their work (similar to ruby koans). This meant creating test cases
for most of the homeworks after we taught functions.  While the idea wasn't
bad, my inexperience made usage a bit obtuse for a long while and probably hurt
more than it helped for a CS1 course.


## turnin.sh
For the course, the way to turnin the homework was to push your code to
github with a given tag. This gave us a clear marker for when something
was turned in and a timestamp for if it was late. To make the process
easier, students could just run `./turnin.sh hwN` to run all the git
commands for them. The system may have been overly complicated for first
year programmers, but I can see something similar being used for a
software engineering course.

# rs - the Racket Sequencer

## Overview

rs is a set of files / library for doing live coding of MIDI sequencing using the [Racket](https://racket-lang.org) programming language. Since Racket is a type of Lisp, sequencing in rs is done by creating lists of events and looping them.

Here's an example in which a boom tsss loop is assigned to a track:

```
(set-rs-t-seq! track1 (list boom
                            '()
                            tsss
                            '()))
```

(a '() is an empty list or null, so the above sequence has four steps, on the first of which an event called "boom" plays and on the third of which an event called "tsss" is played).

Sequences can have arbitrary lengths and the number of items in a sequence is independent of the loop length of a track so it is very easy to play with polymeter or polyrhythm. Sequences can even be nested!

You can have as many tracks, each running their own sequences, each of which can be a different length (theoretically they could even run at different speeds) as your system can handle (if it's too much timing will become sloppier).

Programming happens in Racket, so if you can build it in Racket, you can use it to sequence your MIDI (soft) synths.

For more info, see the Scribble documentation contained in this package. Read it online at:

https://docs.racket-lang.org/rs@rs/index.html

You will probably also want to install [rs-l](https://github.com/mcdejonge/rs-l), a library that contains utilities for sequencing. It makes it easy to do things like making event triggering conditional and rotating sequences. Read about it here:

https://docs.racket-lang.org/rs-l/index.html

If you'd prefer to watch a brief and very bad video that shows how to set things up (on a Mac), go here: https://www.youtube.com/watch?v=GYTyBV1PdXw


## Status

rs is a hobby project, built to scratch an itch for myself. It does work - on my system at least, which is a Macbook with developer tools installed. I've also heard it runs on Linux (Arch). If you're not afraid of compiling some code you may very well get it to work on your system as well.

Sending MIDI messages is done with a Racket wrapper around the [RtMidi library](https://www.music.mcgill.ca/~gary/rtmidi/). I did not write either the wrapper or the library but did manage to get them to work on my system. Getting the wrapper to work requires compiling some code, however. See the installation instructions.

## Installation

### Installing RtMidi

This program needs RtMidi, a Racket wrapper around the RtMidi library. Install it using the Racket package manager (raco pkg install rtmidi).

RtMidi is not a standalone installation. You need download the RtMidi library into the directory where the RtMidi Racket package lives *and compile it*. This works on Linux and on Mac OS X if you have the developer tools installed.

Instructions copied and pasted from the [RtMidi documentation](https://docs.racket-lang.org/rtmidi/index.html):

> This package is not self-contained. It depends on the RtMidi package, and also requires you to compile a dynamic library used to connect to the RtMidi code.
>
> At this point, this means that installing this package will require you to locate the package directory where this package is installed.
>
> The best way to do this is probably to install the package using raco pkg install or the package manager, and then to evaluate
>
> ```(collection-path "rtmidi")```
>
> to see where the directory lives.
>
> Once you’ve located the collection directory, you’ll need to extract http://www.music.mcgill.ca/~gary/rtmidi/release/rtmidi-2.1.0.tar.gz to the collection directory. Then run ‘make $PLATFORM‘, where ‘PLATFORM‘ is one of ‘linux‘, ‘macosx‘, or ‘windows‘.
>
> The wrapper is C++98 and should compile with any modern C++ compiler.
>
> I haven’t tried the Windows build with this Makefile; you might need to make some adjustments.

### Installing rs

Installing rs itself is a simple matter of installing it using the racket package manager (raco pkg install rs) but you can also download the repository you're currently viewing somewhere.

You will also want to install the demos: https://github.com/mcdejonge/rs-demos , if only because this repository contains a starting template.

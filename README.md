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

## Status

rs is a hobby project, built to scratch an itch for myself. It does work - on my system at least, which is a Macbook with developer tools installed. If you're not afraid of compiling some code you may very well get it to work on your system as well.

Sending MIDI messages is done with a Racket wrapper around the [RtMidi library](https://www.music.mcgill.ca/~gary/rtmidi/). I did not write either the wrapper or the library but did manage to get them to work on my system. Getting the wrapper to work took some elbow grease, however. See the installation instructions.

## Performance

Racket is not built for real time computing and I am not a systems programmer so the timing of rs is a bit ... wobbly. If you examine its output in a DAW you will find events are close to where they should be but not exactly on the grid.

You will probably get the best results from running rs in a REPL on the command line. On my own computer, a 2015 Macbook pro with 8GB RAM and a 2,9 GHz Intel Core i5, this yields quite acceptable timing.

When running the REPL inside Emacs performance is slightly worse but still acceptable. This is what I do. rs *can* also run from DrRacket but I've found this causes noticeably sloppier timing.

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

On a Mac, but possibly also on Linux, you may get an error about not being able to find a wrapper-rtmidi file. To fix this error, you need to modify the code of the file ```main.rkt``` in the RtMidi package (you can find out where it lives using the ```(collection-path "rtmidi")``` command you used before).

Go to https://github.com/mcdejonge/rtmidi and find a modified version of ```main.rkt``` in the rtmidi directory: https://github.com/mcdejonge/rtmidi/blob/master/rtmidi/main.rkt

(I've created a pull request on the original repository so maybe by the time you read this you no longer need to do this manually).

### Installing rs

Installing rs itself is a simple matter of installing it using the racket package manager (raco pkg install rs) but you can also download the repository you're currently viewing somewhere.

You will also want to install the demos: https://github.com/mcdejonge/rs-demos , if only because this repository contains a starting template.

## Getting started

1. First, make sure you have at least one available MIDI port on your system that is connected to something that makes a sound. A loopback device to a DAW would be a good choice but hardware should also work.

2. Next, open one of the demo files (available from https://github.com/mcdejonge/rs-demos )in your Racket editing environment of choice and run it. If you've set up everything correctly you should be hearing a simple sequence.

   rs has been tested with DrRacket and with Emacs running racket-mode. When using DrRacket, make sure to disable debugging (Language -> Choose Language -> The Racket Language -> No debugging or profiling). Even with debugging disabled timing in DrRacket is quite sloppy (see "Performance" above) so your best bet is probably to run rs in a REPL in a terminal and to copy and paste commands into it from your editor of choice.

3. Time to start exploring. There are a couple of demo files you can examine to see how rs works. There's also a file called rs-live.rkt that you can load and modify to start doing live coding.

Have fun!

## Changelog

* **2020-05-14** Provided Scribble documentation.

* **2020-05-13** Turned rs into a package.

* **2020-05-12**
  * **New feature**: chords
  
    Use rs-m-play-chord and rs-m-event-play-chord to play multiple notes simultaneously on the same instrument:
    
  ```racket
  (define chord (rs-m-event-play-chord instr (list 60 65 69))
  ```
  
  * Added polyrhythm demo.
 
* **2020-05-10**
  * **New feature**: Sequences within sequencesa
  
    You can now nest sequences, like this:
  
  ```racket
  (define hihats (list hc
                       ho
                       hc
                       (list hh hh hh)))
  ```
    
    A new demo, rs-demo-drums.rkt shows off this feature.
  * Timing code has been slimmed down, which hopefully increases performance.
  * rs-util.rkt now exposes (rs-util-set-diag-mode #t|#f) and (rs-util-diag message .args ) so you can have an idea about what rs is doing.
  * Demos have been put into a separate directory.
* **2020-05-03** Some cleanup of the demos
* **2020-05-01** Initial release


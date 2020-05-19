#lang scribble/manual
@require[@for-label[rs
                    racket/base]]

@title{rs - the Racket Sequencer}
@author{mcdejonge}

@defmodule[rs]


@section{Overview}

rs - the Racket Sequencer - is a live coding tool that lets you sequence MIDI
using Racket. A sequence is a simple list of events, you can play multiple
sequences simultaneously and sequences can have different lengths and
subdivisions so it's easy to do complex polyrhythms and Euclidean sequencing.

Here's an example in which a boom tsss loop is assigned to a track:

@racketblock[
(set-rs-t-seq! track1 (list boom
                            '()
                            tsss
                            '()))
]

A @racket['()] is an empty list or @code{null}, so this
sequence has four steps, on the first of which an event called "boom" plays and
on the third of which an event called "tsss" is played. 

Sequences can have arbitrary lengths and the number of items in a sequence is
independent of the loop length of a track so it is very easy to play with
polymeter or polyrhythm. Sequences can even be nested!

You can have as many tracks, each running their own sequences, each of which
can be a different length (theoretically they could even run at different
speeds) as your system can handle (if it's too much timing will become
sloppier).

Programming happens in Racket, so if you can build it in Racket, you can use it to sequence your MIDI (soft) synths.

@local-table-of-contents[]

@section{Status}

rs is a hobby project, built to scratch an itch for myself. It does work - on my system at least, which is a Macbook
with developer tools installed. If you're not afraid of compiling some code you may very well get it to work on your system too. I've been told it works without problems on (Arch) Linux as well.

Sending MIDI messages is done with a Racket wrapper around the
@hyperlink["https://www.music.mcgill.ca/~gary/rtmidi/" "RtMidi library"].  I
did not write either the wrapper or the library but did manage to get them to
work on my system. Getting the wrapper to work took some elbow grease,
however. See the installation instructions.

@section{Performance}

Racket is not built for real time computing and I am not a systems programmer so the timing of rs is a bit ... wobbly. If you examine its output in a DAW you will find events are close to where they should be but not exactly on the grid.

You will probably get the best results from running rs in a REPL on the command line. On my own computer, a 2015 Macbook pro with 8GB RAM and a 2,9 GHz Intel Core i5, this yields quite acceptable timing.

When running the REPL inside Emacs performance is slightly worse but still acceptable. This is what I do. rs *can* also
run from DrRacket but I've found this causes noticeably sloppier timing.

@section{Installation}

@subsection{Installing RtMidi}

This program needs RtMidi, a Racket wrapper around the RtMidi library. Install it using the Racket package manager (raco pkg install rtmidi).

RtMidi is not a standalone installation. You need download the RtMidi library into the directory where the RtMidi Racket
package lives *and compile it*. This works on Linux and on Mac OS X if you have the developer tools installed.

Instructions copied and pasted from the @hyperlink["https://docs.racket-lang.org/rtmidi/index.html" "RtMidi documentation"]:
@nested[#:style 'inset]{
This package is not self-contained. It depends on the RtMidi package, and also requires you to compile a dynamic library used to connect to the RtMidi code.

At this point, this means that installing this package will require you to locate the package directory where this package is installed.

The best way to do this is probably to install the package using raco pkg install or the package manager, and then
 to evaluate @racket[(collection-path "rtmidi")] to see where the directory lives.

Once you’ve located the collection directory, you’ll need to extract
 @hyperlink["http://www.music.mcgill.ca/~gary/rtmidi/release/rtmidi-2.1.0.tar.gz" "http://www.music.mcgill.ca/~gary/rtmidi/release/rtmidi-2.1.0.tar.gz"]
 to the collection directory. Then run @code{make $PLATFORM}, where @code{PLATFORM} is one of @code{linux}, @code{macosx}, or @code{windows}.

The wrapper is C++98 and should compile with any modern C++ compiler.

I haven’t tried the Windows build with this Makefile; you might need to make some adjustments.
}

@subsection{Installing rs}

Installing rs itself is a simple matter of installing it using the racket package manager (@code{raco pkg install rs}).

You will also want to install the demos: @url{https://github.com/mcdejonge/rs-demos} , if only because this repository contains a starting template.

@section{Getting started}

1. First, make sure you have at least one available MIDI port on your system that is connected to something that
makes a sound. A loopback device to a DAW would be a good choice but hardware should also work.

2. Next, open one of the demo files (available from @url{https://github.com/mcdejonge/rs-demos} in your Racket editing
environment of choice and run it. If you've set up everything correctly you should be hearing a simple sequence.

   rs has been tested with DrRacket and with Emacs running racket-mode. When using DrRacket, make sure to disable
debugging (Language -> Choose Language -> The Racket Language -> No debugging or profiling). Even with debugging
disabled timing in DrRacket is quite sloppy (see "Performance" above) so your best bet is probably to run rs in
a REPL in a terminal and to copy and paste commands into it from your editor of choice.

3. Time to start exploring. There are a couple of demo files you can examine to see how rs works. There's also a file
called rs-live.rkt that you can load and modify to start doing live coding.

Have fun!
@section[#:tag "functions"]{Functions}

This section lists all the functions that are available to do your live coding with.

@subsection[#:tag "functions-global"]{Global / Main}

These functions and values deal with the global environment and the main loop.

The following values contain the current settings for the main loop:

@defthing[rs-main-bpm natural?]{
The currently set main loop BPM.
}

@defthing[rs-main-div-length positive?]{
The currently set main loop division length.
}

@defthing[rs-main-steps natural?]{
The currently set number of steps in the main loop.
}

These functions can be used to alter the settings for the main loop:

@defproc[(rs-set-global-bpm! [bpm natural?]) void]

Set the global BPM.

@defproc[(rs-set-global-div-length! [div-length positive?]) void]

Set the global division length. Division length is a multiplier of the main beat length, so a division length of 1/4 means there will be one step every quarter beat.

@defproc[(rs-set-global-steps! [steps natural?]) void]

Set the global number of steps per iteration of the main / global loop.

Use these functions to start and stop the main loop:

@defproc[(rs-start-main-loop!) void]

Start the main loop.

@defproc[(rs-stop-main-loop!) void]

Stop the main loop.

@subsection[#:tag "functions-tracks"]{Tracks}

These functions deal with creating, queueing and stopping tracks.

Starting (queueing) and stopping tracks:

@defproc[(rs-queue-track! [track rs-t?]) void]
Enqueues the given track. It will be started on the next iteration of the main loop.

@defproc[(rs-stop-track! [track natural-or-track?]) void]

Stops the given track, which can by either an rs-t struct or an index (useful when you want to stop the last track you started or somesuch). Stopping happens at the start of the next iteration of the main loop.

Creating tracks:

@defproc[(rs-track [sequence list?]) rs-t]

Creates a new track that uses the global settings for BPM, number of steps and division length. sequence should be a list where each element is either null, an event (rs-e?) or a valid sequence.

@defproc[(rs-t-create [#:bpm bpm positive?]
                      [#:steps steps positive? 16]
                      [#:div-length div-length positive? 1/4]
                      [#:seq seq list? '()]) rs-t?]

Creates a new track but allows you to set some (or all) of the track settings manually.

@defstruct[rs-t ([bpm positive?]
                 [steps positive?]
                 [div-length positive?]
                 [seq list?])]
Represents a track. Stores tempo, number of steps and division length as well as a sequence, which is a list of elements that can be either null, an event (rs-e) or another sequence.


@subsection[#:tag "functions-events"]{Events}

Sequences consist of lists of events (null is also an event, albeit on in which nothing happens). An event in which something does happen is an rs-e structure, which combines a function to execute and an offset (optional, defaults to 0 but can be any number between -1 and +1 which is then multiplied by the length of the step it applies to and added from the length of the step).

@defstruct[rs-e ([fn procedure-list-or-null]
                 [offset number?])]
The structure that contains an event. Offset should be a number between -1 and +1 and represents how far the event will take place from its regular position in the sequence. -1 is the start of the previous item in the sequence and +1 is the start of the next item in the sequence. fn can be either null, a sequence (that is a list of null, events or, gasp, sequences) or a function that should accept one argument. That one argument is the length of the step on which the event is executed, in milliseconds.

It's advisable to avoid creating rs-e events directly and instead use:

@defproc[(rs-e-create [#:fn fn procedure?]
                      [#:offset offset number? 0]) rs-e]
Create a new event. Does sanity checking.

If you want to fire off multiple events simultaneously, use:

@defproc[(rs-e-multiple [procedures list?]) procedure]

Creates a function that will fire off multiple event procedures at the same time (and at the same offset).

@subsection[#:tag "functions-midi"]{MIDI functions}

These functions can be used to work with MIDI: create MIDI events, define MIDI instruments and determine which ports are available.

To check which ports are available use:

@defproc[(rs-m-list-ports) list]

Returns a list of available MIDI ports on your system. This list contains the names of the ports.

To define a MIDI instrument use:

@defproc[(rs-m-instr [port valid-midi-port?]
                     [channel valid-midi-channel? 1])
         rs-m-instr-struct]

Creates a new MIDI instrument on the supplied MIDI port index and on the supplied MIDI channel number. It returns a struct that can be used in the MIDI event functions listed below.

To create events that play MIDI notes, use these functions:

@defproc[(rs-m-event-play [instr rs-m-instr-struct?]
                          [note rs-m-valid-midi-value?]
                          [note-length-ms natural?]
                          [velocity rs-m-valid-midi-value? 127]
                          [#:offset valid-offset? 0]) rs-e]

Create a MIDI event for use in a sequence that plays the given note of the given length and with the given velocity using the given instrument and offset.

Should you want to play your MIDI note directly instead of using it in a sequence use this function:

@defproc[(rs-m-play [instr rs-m-instr-struct?]
                    [note rs-m-valid-midi-value?]
                    [note-length-ms natural?]
                    [velocity rs-m-valid-midi-value? 127]) void]
Directly play the given note for the given length using the given velocity on the given instrument.

@defproc[(rs-m-event-play-chord [instr rs-m-instr-struct?]
                                [notes rs-m-valid-notes?]
                                [note-length-ms natural?]
                                [velocity rs-m-valid-midi-value? 127]
                                [#:offset valid-offset? 0]) rs-e]

Returns a MIDI event for use in a sequence that plays all notes in the given list of notes simultaneously for the given amount of time, at the given velocity and using the given instrument (and at the given offset).

Should you want to play your chord directly instead of using it in a sequence use this function:

@defproc[(rs-m-play-chord [instr rs-m-instr-struct?]
                          [notes rs-m-valid-notes?]
                          [note-length-ms natural?]
                          [velocity rs-m-valid-midi-value? 127]) void]

Directly play the given notes simultaneously for the given amount of time, at the given velocity and using the given instrument.

It is also possible to set MIDI cc values. Use these functions:

@margin-note{The rs-demos package at @url{https://github.com/mcdejonge/rs-demos} contains a tool for setting up MIDI cc listeners in your DAW / on your instument.
 }
@defproc[(rs-m-event-cc [instr rs-m-instr-struct?]
                         [cc-no rs-m-valid-midi-value?]
                         [cc-val rs-m-valid-midi-value?]
                         [#:offset valid-offset? 0]) rs-e]
Returns a MIDI event for use in a sequence that sets the given cc number to the given value for the given instrument. Supply an offset as needed.

Should you want to set the cc value directly instead of doing so in a sequence
@defproc[(rs-m-cc [instr rs-m-instr-struct?]
                  [cc-no rs-m-valid-midi-value?]
                  [cc-val rs-m-valid-midi-value?]) rs-e]

Set the given cc number to the given value for the given instrument. Supply an offset as needed.


@section{Changelog}

* @bold{2020-05-17} Implemented offsets.

* @bold{2020-05-14} Created Scribble documentation (with help from Stephen De Gabrielle because I'm a Racket noob).
* @bold{2020-05-13} Turned rs into a package.

* @bold{2020-05-12}

*  * @bold{New feature}: chords
  
    Use rs-m-play-chord and rs-m-event-play-chord to play multiple notes simultaneously on the same instrument:
    
@racketblock[
  (define chord (rs-m-event-play-chord instr (list 60 65 69)))
]
  
*  * Added polyrhythm demo.
 
* @bold{2020-05-10}

*  * @bold{New feature}: Sequences within sequences
  
    You can now nest sequences, like this:
  
@racketblock[
  (define hihats (list hc
                       ho
                       hc
                       (list hh hh hh)))
]
    
    A new demo, rs-demo-drums.rkt shows off this feature.

*  * Timing code has been slimmed down, which hopefully increases performance.

*  * rs-util.rkt now exposes (rs-util-set-diag-mode #t|#f) and (rs-util-diag message .args ) so you can have an idea about what rs is doing.

*  * Demos have been put into a separate directory.

* @bold{2020-05-03} Some cleanup of the demos

* @bold{2020-05-01} Initial release


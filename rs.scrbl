#lang scribble/manual

@(require scribble/manual)

@title{rs - the Racket Sequencer}

@author[(author+email "Matthijs de Jonge"
                      "matthijs@rommelhok.com")]

@(require(for-label racket
                    "main.rkt"
                    "rs-e.rkt"
                    "rs-m.rkt"
                    "rs-t.rkt"))

@defmodule[rs]{rs - the Racket Sequencer - is a live coding tool that lets you sequence MIDI using Racket. A sequence is a simple list of events, you can play multiple sequences simultaneously and sequences can have different lengths and subdivisions so it's easy to do complex polyrhythms and Euclidean sequencing.}

@local-table-of-contents[]

@section[#:tag "functions"]{Functions}

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

@defproc[(rs-set-global-bpm! [bpm natural?]) void?]

Set the global BPM.

@defproc[(rs-set-global-div-length! [div-length positive?]) void?]

Set the global division length. Division length is a multiplier of the main beat length, so a division length of 1/4 means there will be one step every quarter beat.

@defproc[(rs-set-global-steps! [steps natural?]) void?]

Set the global number of steps per iteration of the main / global loop.

Use these functions to start and stop the main loop:

@defproc[(rs-start-main-loop!) void?]

Start the main loop.

@defproc[(rs-stop-main-loop!) void?]

Stop the main loop.

@subsection[#:tag "functions-tracks"]{Tracks}

These functions deal with creating, queueing and stopping tracks.

Starting (queueing) and stopping tracks:

@defproc[(rs-queue-track! [track rs-t?]) void?]
Enqueues the given track. It will be started on the next iteration of the main loop.

@defproc[(rs-stop-track! [track-no natural?]) void?]

Stops the track with the supplied index. Stopping happens at the start of the next iteration of the main loop.

Creating tracks:

@defproc[(rs-track [sequence list?]) rs-t?]

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

Sequences consist of lists of events (null is also an event, albeit on in which nothing happens). An event in which something does happen is an rs-e structure, which combines a function to execute and an offset (which isn't used, yet, but will be in a future release).

@defstruct[rs-e ([fn procedure?]
                 [offset number?])]
The structure that contains an event. Offset should be a number between -1 and +1 and represents how far the event will take place from its regular position in the sequence. -1 is the start of the previous item in the sequence and +1 is the start of the next item in the sequence. NOTE: offset functionality is not yet implemented. fn can be either null or a function that should accept one argument. That one argument is the length of the step on which the event is executed, in milliseconds.

It's advisable to avoid creating rs-e events directly and instead use:

@defproc[(rs-e-create [#:fn fn procedure?]
                      [#:offset offset number?]) rs-e?]
Create a new event. Does sanity checking.

If you want to fire off multiple events simultaneously, use:

@defproc[(rs-e-multiple [procedures list?]) procedure?]

Creates a function that will fire off multiple event procedures at the same time (and at the same offset).

@subsection[#:tag "functions-midi"]{MIDI functions}

These functions can be used to work with MIDI: create MIDI events, define MIDI instruments and determine which ports are available.

To check which ports are available use:

@defproc[(rs-m-list-ports) list?]

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

Should you want to play your MIDI note directly instead of using it in a sequence, use this function:

@defproc[(rs-m-play [instr rs-m-instr-struct?]
                    [note rs-m-valid-midi-value?]
                    [note-length-ms natural?]
                    [velocity rs-m-valid-midi-value? 127]) void]
Directly play the given note for the given length using the given velocity on the given instrument.

TODO document
rs-m-event-cc
rs-m-event-play-chord
rs-m-cc
rs-m-play-chord



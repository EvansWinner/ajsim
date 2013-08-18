# ajsm.lisp --- Advanced Job Scheduler Model

`ajsm.lisp` provides a model of a subset of IBM's Advanced Job
Scheduler for the IBM i system -- namely, the subset which I actually
use. The threshold function is not modeled.  It is not modeled because
I don't use it.  I don't use it because neither I nor IBM knows what
it does nor how it works nor what is it is for.  Other than that, it
seems to work.

Of course, the cardinality of the set which is the intersection of the
set of people who use AJS and the set of people who are comfortable
using Common Lisp is probably one -- or, in the Queen's English, I'm
probably the only one who would ever use this thing.  But you never
know, so I am putting it online.

## Purpose

The purpose is to test job schedules (timing, dependencies, and so
forth) in advance of actually implementing them on a live system.  It
is possible to simulate other job schedulers with it -- in particular
the IBM i built in "WRKJOBSCDE" scheduler is easy to model with this
system, because it's functionality is basically a subset of AJS's
functionality.  In fact, you can model WRKJOBSCDE jobs and AJS jobs at
the same time.  Just add the job definitions and they all run
together.

## Requirements

`ajsm.lisp` is pretty simple and should run on any conforming Common
Lisp implementation.

If you would like to use the functions that export your graph (the
graph of scheduled jobs) to the Graphviz .dot format and render them
then naturally you will need Graphviz installed as well.  This
functionality requires Common Lisp libraries `s-dot` and
`trivial-shell`.  These are loaded right at the top of `ajsm.lisp`
using Quicklisp.

So, to recap, if you have Graphviz and Quicklisp installed, then you
are ready to go.  If you are missing either of those, then you will
want to comment out those two `(ql:quickload`... lines.

## Installation

After seeing the previous section, load the file in your Common Lisp
implementation.  Ta-da!

## Use

After loading it, you might want to 

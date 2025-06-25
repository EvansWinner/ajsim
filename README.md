# ajsm.lisp --- IBM i Advanced Job Scheduler Simulator

[Note: It's a simulator, not a model. A simulation IMPLEMENTS a model.
 So, I changed the name of the repository, but haven't changed the file
 names.]

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
`trivial-shell`.  These WERE loaded right at the top of `ajsm.lisp`
using Quicklisp.

**Added June, 2025**: But now s-dot appeares no longer to be available
from Quicklisp. Therefore, here is a recipe to get the example file
to load:

 1. Download http://martin-loetzsch.de/S-DOT/s-dot.tar.gz and put the
    file somewhere you like. In that directory, run =tar xvzf s-dot.tar.gz=
 2. From the resulting directory, move the file s-dot.lisp to the same
    place you indent to run ajsim from.
 3. In that directory, run =sbcl --load s-dot.list --load example-schedule.lisp=
 4. Do =(in-package :ajsm)= and then =(example-do-december-2012)= and you will
    see the output from the example run.

So, to recap, if you have Graphviz and Quicklisp installed, then you
are ready to go.  If you are missing either of those, then you will
want to comment out those two `(ql:quickload`... lines.

## Installation

After seeing the previous section, load the file in your Common Lisp
implementation.  Ta-da!

## Use

Take a look at example-schedule.lisp.  It loads `ajsm.lisp`, so you
can just load that file and you will be in the ajsm package and ready
to run the simulation.

A set of jobs is a directed graph.  The vertices are all Boolean
values.  They may represent a job which must run, or a job which must
run before another job runs; they may represent a time of day which it
must be for a job to run, or a day of the week, or a complex calendar
which must in effect say, "yes, you can run today."  Mind, now -- they
are ALL just vertices on the graph.

The simulation simply cycles through all the nodes on the graph, and
for each node, checks to see if all the nodes which lead to it are set
to a Boolean true value.  If so, it sets that node to true.  "Setting
it to true" is the same thing is "running" the job.  Once the job has
"run" then it is "true" for the purposes of any and all nodes (jobs)
which depend on it.  Make sense?

(So as it happens, there is a facility which allows you to actually run
arbitrary Lisp code when a "job" (i.e., a vertex) is "run" (i.e., set
TRUE).  So, using that, you could actually use the model of a job
scheduler as a real job scheduler -- but you would have to write some
code to hook it into some kind of clock to actually use it for realz.)

So the questions are basically two, then: 1) how do you define a
schedule, and 2) how do you run through it.

### How to define a job schedule

Well, first, check `example-schedule.lisp` and take a look at how jobs
are defined.  Then look at the code in `ajsm.lisp` because there are
some comments, some doc strings, and more importantly, comments above
most of the functions and macros which have examples of the syntax.
Personally I find that helpful.

### How to run the simulation

Well, first, check `example-schedule.lisp` and run function
`example-do-december-2012' and you will see a run.

## Export to Graphviz format

You can export your graph to Graphviz' .dot format and from there
render it as a .pdf or .tiff file or whatever you like to visualize
it.  Now that's sort of fun, isn't it?

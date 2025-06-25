;;;; ajsm.lisp --- simulate IBM's Advanced Job Scheduler (AJS) for System i in Lisp

;;;; AUTHOR: Evans Winner
;;;; DATE: <2012-02-22 Wed>
;;;; COPYRIGHT: 2012, Evans Winner

;;;; Commentary

;;;; This program provides a model of a subset of AJS -- the subset
;;;; which I actually use. The threshold function is not modeled.  It
;;;; is not modeled because I don't use it.  I don't use it because
;;;; neither I nor IBM knows what it does nor how it works nor what is
;;;; it is for.  Other than that, it seems to work.

;;;; If your only hammer is Lisp, then every problem will look like a
;;;; list.  Er, that's my way of saying that in retrospect, I should
;;;; have used structs.  For all the use of `loop' and `eval' I have
;;;; no excuses.

;; You must have quicklisp loaded first for this to work
;(ql:quickload "s-dot")			;optional -- for rendering the
                                        ;graph. This is no longer load-
                                        ;able with quicklisp.
(ql:quickload "trivial-shell")

(in-package :cl-user)
(defpackage "ADVANCED-JOB-SCHEDULER-MODEL"
  (:nicknames "AJSM")
  (:use "COMMON-LISP-USER")
  (:use "COMMON-LISP")
  ;; Not sure about this. I ended up just doing everything from within
  ;; the package.
  (:export n
   #:do-day
   #:bulk-add-vertices
   #:bulk-create-vertices
   #:make-nodes
   #:run-and-go
   ))
(in-package :ajsm)

(defparameter *graph* nil)
(defvar off nil)
(defvar on t)
;; Vertex types -- pretty much always optional, but I might do
;; something with them.  Used only in export to s-dot now
(defvar job 'job)
(defvar group 'group)
(defvar time-of-day 'time-of-day)
(defvar scal 'scal) ;scheduling calendar
(defvar hcal 'hcal) ;holiday calendars
(defvar days 'days)

(defvar *run-jobs* nil
  "Run function or functions associated with jobs when run.  This
allows the model of a job scheduler to actually BE a job scheduler,
except hooking it up with a clock function is your problem.  See
functions `add-job' and `run-job' below for more.")

;;; Basic functions and data structures

;;; If I had more brains I would have made jobs into structs instead
;;; of doing the obvious lispy thing of making everything with lists
;;; and plists and such.  Oh well; live and learn.

(defmacro clear-graph (graph)
  "Just set the graph variable to nil.  Created vertices still exist."
  `(set ',graph nil))

(defmacro make-dep (list)
  "Given a list of predecessors, make an alist consisting of (job
fulfilled-p)."
  `(loop for job in ,list collect
       (cons job 'off)))

;; Eg. (new-vertex 'group1 '(job1 job2) 'group)
(defun new-vertex (name predecessors &optional (type 'job) functs)
  "Create a new vertex and return its name.  If list FUNCTS is
specified, add each function in the list to the job as functions to
run with `run-job' when the vertex is run."
  (set name
       (list (make-dep predecessors)
             type))
  (if functs
      (loop for function in functs do
	   (add-job name function)))
  name)                       ;return the symbol

;; Eg. (add-vertex *mygraph*
;;         (new-vertex 'group1 '(job1 job2) 'group)
(defmacro add-vertex (graph vertex)
  `(if (member ,vertex ,graph)
       (progn 
         (format t "Vertex already exists in graph~%")
         nil)
       (progn (pushnew ,vertex ,graph)
              ',vertex)))

;; Eg. (vertex-predecessors jobq)
(defun vertex-predecessors (vertex)
  (car vertex))

;; Eg. (vertex-type jobq)
(defun vertex-type (vertex)
  (cadr vertex))

;; Eg. (pprint-vertex 'group1)
(defun pprint-vertex (vertex)
  (format t "Vertex: ~A~%Predecessor/state list: ~A~%Type: ~A~%~%"
          vertex
          (vertex-predecessors (symbol-value vertex))
          (vertex-type (symbol-value vertex))))

;; Eg. (pprint-graph *mygraph*)
(defun pprint-graph (graph)
  (loop for vertex in graph do
       (pprint-vertex vertex)))

;; Eg (remove-vertex *mygraph* 'job1) I don't actually use this much.
;; Instead I just edit the big list I process with `bulk-add-vertices'
;; and just start over from scratch when I want to make a change.  But
;; who knows.  Maybe this could be useful.
(defmacro remove-vertex (graph vertex)
  "Remove VERTEX from GRAPH and return the new graph."
  `(if (not (member ,vertex ,graph))
       (progn (format t "Vertex does not exist in graph~%")
              nil)
       (set ',graph (delete ,vertex ,graph))))

;; Eg. (set-vertex-type job1 'group)
;; Same thing.  Don't actually use this much.
(defun set-vertex-type (vertex type)
  (setf (cadr vertex) type))

;; Eg. (set-vertex-predecessors job1 '(job2 job3))
(defun set-vertex-predecessors (vertex predecessors)
  (setf (car vertex) (make-dep predecessors)))

;;; Actual jobs

;; Eg. (add-job 'nightlyjob 'function-name)
;; Note that you can also sharp-quote, but that will add a copy of the
;; acutal function object to the plist, so if you redefine your
;; function later, the old version will still get run by your job.
(defun add-job (vertex function)
  "Add a function to a job.  

This adds a function 'FUNCTION-NAME to a list of such functions kept
under the plist key FUNCTS of the symbol plist of symbol JOB which is
assumed to be a job defined as such.  If variable *run-jobs* is T then
function `run-vertex' will funcall any such functions one at a time.
If one errors, the whole shebang errors, so... use with care.  This
could be useful for, for example, debugging the simulation -- when a
vertex runs it can be set to print a special message, for example.
Or, if you're crazy, you could set a job to run an actually useful
function and use AJSM as an actual job scheduler.  Stranger things
have been done."
  (setf (get vertex 'funcs) (cons function (get vertex 'funcs))))

(defun run-job (vertex)
  (loop for func in (get vertex 'funcs) do
       (funcall func)))

(defun clear-jobs (vertex)
  (setf (get vertex 'funcs) nil))

(defun show-jobs (vertex)
  (get vertex 'funcs))

;;; Simulation engine

;; Eg. (get-predecessor-state job1 'job2)
(defun get-predecessor-state (vertex predecessor)
  (cdr (assoc predecessor (vertex-predecessors vertex))))

;; Eg. (set-predecessor-on job1 'job2)
(defun set-predecessor-on (vertex predecessor)
  (setf (cdr (assoc predecessor (vertex-predecessors vertex))) 'on))

;; Eg. (set-predecessor-off job1 'job2)
(defun set-predecessor-off (vertex predecessor)
  (setf (cdr (assoc predecessor (vertex-predecessors vertex))) 'off))

;; Eg. (reset-graph *mygraph*)
(defmacro reset-graph (graph)
  `(loop for vertex in ,graph do
       (reset-vertex-predecessors (symbol-value vertex))))

;; Eg. (list-vertex-predecessors-sans-state job1)
;; Helper function for `set-predecessor-flags.
(defun list-vertex-predecessors-sans-state (vertex)
  "Get all predecessors of VERTEX as a list, without the state."
  (loop for predecessor in (vertex-predecessors vertex) collect
       (car predecessor)))

;; Eg. (list-vertex-predecessors-sans-predecessors job1)
(defun list-vertex-predecessors-sans-predecessors (vertex)
  (loop for predecessor in (vertex-predecessors vertex) collect
       (cdr predecessor)))

;; Eg. (reset-vertex-predecessors job1)
(defun reset-vertex-predecessors (vertex)
  "Reset the predecessor list for VERTEX."
  (loop for predecessor in (list-vertex-predecessors-sans-state vertex) do
       (set-predecessor-off vertex predecessor)))

;; Eg. (run-vertex *mygraph* 'job1)
(defun run-vertex (graph vertex)
  "Loop though all vertices in GRAPH and for each vertex which
contains VERTEX as a predecessor, set its flag as ON."
  (format t "Running: ~A <== ~A~%" vertex (vertex-predecessors (symbol-value vertex)))
  (loop for item in graph do
       (if (member vertex (list-vertex-predecessors-sans-state (symbol-value item)))
	   (progn (set-predecessor-on (symbol-value item) vertex)
		  (if *run-jobs* (run-job vertex))))
       (reset-vertex-predecessors (symbol-value vertex))))

;; Eg. (unrun-vertex *mygraph* 'job1)
(defun unrun-vertex (graph vertex)
  "Loop though all vertices in GRAPH and for each vertex which
contains VERTEX as a predecessor, set its flag as OFF.  Used for time
and day unsetting for looping through time."
  (if vertex
      (progn  (format t "Resetting: ~A~%" vertex)
              (let ((it (vertex-predecessors (symbol-value vertex))))
                (if it
                    (format t "Unsetting: ~A <== ~A~%" vertex it)))
              (loop for item in graph do
                   (if (member vertex (list-vertex-predecessors-sans-state (symbol-value item)))
                       (set-predecessor-off (symbol-value item) vertex))
                   (reset-vertex-predecessors (symbol-value vertex))))))

(defun and-list (list)
  (let ((temp t))
  (loop for item in list do
       (if (null item)
           (setq temp nil)))
  temp))

;; Eg (vertex-origin-p job1)
(defun vertex-origin-p (vertex)
  (if (null (vertex-predecessors vertex))
      t nil))

;; Eg. (vertex-ready-p job1)
(defun vertex-ready-p (vertex)
  (and-list
   (loop for state in 
        (list-vertex-predecessors-sans-predecessors vertex)
      collect (symbol-value state))))

(defun step-graph (graph)
  (let ((something-happened nil))
  (loop for vertex in graph do
       (if (and (not (vertex-origin-p (symbol-value vertex)))
                (vertex-ready-p (symbol-value vertex)))
           (progn
             (run-vertex graph vertex)
             (setq something-happened t))))
  something-happened))

(defun spin-graph (graph)
  "Run the graph until all possible vertices have run."
  (when (step-graph graph)
    (spin-graph graph)))

;; Eg. (back-assoc 1900 *times-alist*)
(defun back-assoc (item &optional (alist *times-alist*))
  "For a datum, return the sumbol that is its key."
  (if (equal alist nil) nil
      (if (equal (cdr (car alist)) item)
	  (car (car alist))
	  (back-assoc item (cdr alist)))))
  
;; Eg. (run-at-time 1900 *mygraph* *times-alist*)
(defun run-at-time (time &optional (graph *graph*) (time-list *times-alist*))
  (let ((time (back-assoc time time-list)))
    (if time
	(run-vertex graph time))))

;; Eg. (run-at-day *mygraph* 'monday *day-groups*)
(defun run-at-day (graph day days-list)
  (let ((day-vertices
	 (loop for group in days-list collect
	      (if (member day group) (car group)))))
    (loop for group in day-vertices do
	 (if group (run-vertex graph group)))))

(defmacro reset-days (graph days-list)
  `(loop for list in ,days-list do
        (unrun-vertex ,graph `,(car list))))

(defun do-day (graph &optional
		 (weekday 'monday)
		 (days-list *day-groups*)
		 (time-list *times-alist*)
		 (start-time 0))
  (reset-days graph days-list)
  (run-at-day graph weekday days-list)
  ;; Doesn't really matter that nothing is defined to run at 68
  ;; minutes after the hour, etc.
  (loop for minute from start-time to 2359 do
       (progn
	 (spin-graph graph)
	 (run-at-time minute graph time-list)
	 (spin-graph graph)
	 (unrun-vertex graph (back-assoc minute time-list)))))

;;; Convenience

;; Eg. (run-and-go *mygraph* 'job1)
;; or  (run-and-go *mygraph* '(job1 job2))
;; to initialize a graph the way you want it:
;; (run-and-go *mygraph* *init-state*) ; where *init-state* is a list of
;; vertices you want to set on
(defun run-and-go (graph vertices)
  "Run a vertex or a list of vertices, then spin the graph."
  (if (symbolp vertices)
      (run-vertex graph vertices)
      (loop for vertex in vertices do
	   (run-vertex graph vertex)))
  (spin-graph graph))

;; Eg. (bulk-create-vertices
;;      '((job1 (job2 job3)) ;default type = 'job
;;        (group1 (job1 job3) group)))              
(defun bulk-create-vertices (vertices)
  (loop for vertex in vertices
     collect
       (new-vertex (first vertex)
		   (second vertex)
		   (if (third vertex) (third vertex) 'job)
		   (if (fourth vertex) (fourth vertex)))))

;; Eg. (bulk-add-vertices '*mygraph*
;;                     '((job1 (job2 job3))
;;                       (job2 (job3 job4))))
(defun bulk-add-vertices (graph vertices)
  (loop for vertex in 
       (bulk-create-vertices vertices)
       do
       (add-vertex graph vertex))
  graph)

;;; Export graph to s-dot format

(defparameter *format* "pdf")
(defparameter *s-dot-output-file* (concatenate 'string "output." *format*))
(defvar *s-dot-graph* nil)

(defun make-node (vertex)
  (let ((node (symbol-name vertex))
        (type (eval (vertex-type (symbol-value vertex)))))
    (list 's-dot::node
          (list (list 's-dot::id node)
                (list 's-dot::label node)
                (list 's-dot::color
		      (cond 
			((equal (symbol-name type) "JOB") "black")
			((equal (symbol-name type) "GROUP") "green")
			((equal (symbol-name type) "SCAL") "blue")
			((equal (symbol-name type) "HCAL") "blue")
			((equal (symbol-name type) "DEV") "red")
			((equal (symbol-name type) "TIME-OF-DAY") "purple")
			((equal (symbol-name type) "DAYS") "blue")
			((equal (symbol-name type) "OR") "yellow")
			(t "black")))
		(list 's-dot::shape 
		      (cond 
			((equal (symbol-name type) "JOB") "ellipse")
			((equal (symbol-name type) "GROUP") "box")
			((equal (symbol-name type) "SCAL") "trapezium")
			((equal (symbol-name type) "HCAL") "invtrapezium")
			((equal (symbol-name type) "DEV") "box3d")
			((equal (symbol-name type) "TIME-OF-DAY") "pentagon")
			((equal (symbol-name type) "DAYS") "trapezium")
			((equal (symbol-name type) "OR") "triangle")
			(t "box")))))))

(defun make-nodes (graph)
  (loop for node in graph collect
       (make-node node)))

(defun make-edge (from-vertex to-vertex)
  (let ((from (symbol-name from-vertex))
	(to (symbol-name to-vertex)))
    (list 's-dot::edge
	  (list
	   (list 's-dot::from from)
	   (list 's-dot::to to)))))

;; Eg (make-edges-for-vertex 'foo)
(defun make-edges-for-vertex (vertex)
  (loop for predecessor in (vertex-predecessors (symbol-value vertex))
       collect (make-edge (car predecessor) vertex)))

(defun make-edges-for-graph (graph)
  (let ((result nil))
    (loop for vertex in graph do
	 (loop for edge in 
	      (make-edges-for-vertex vertex) do
	      (setq result (cons edge result))))
    result))

(defun make-s-dot-graph-sans-header (graph)
  (let ((rtn nil))
    (loop for item in (make-edges-for-graph graph) do
	 (setq rtn (cons item rtn)))

    (loop for item in (make-nodes graph) do
	 (setq rtn (cons item rtn)))
    rtn))

;; Eg. (let ((*s-dot-graph* (make-s-dot-graph *mygraph* "My Graph" 'letter)))
;;       (process-dot-file))
(defun make-s-dot-graph (graph &optional (title "AJS") (size 'letter))
  (let ((paper 
	 (cond ((equal size 'letter) "11,8.5")
	       ((equal size 'legal) "14,11")
	       ((equal size 'double) "17,11")
	       (t "8.5,11"))))
    (cons 's-dot::graph 
	  (cons `((s-dot::label ,title) (s-dot::size ,paper)
                  (s-dot::rankdir "LR")
                  )
		(make-s-dot-graph-sans-header graph)))))

(defun make-dot-file (filename graph)
  (s-dot::render-s-dot filename *format* graph))

(defun process-dot-file (&optional outfile s-dot-graph)
  (unless
      (make-dot-file (or outfile *s-dot-output-file*) (or s-dot-graph *s-dot-graph*))
    'success))

			     
;;; Convenience

;; TODO: Write tapes-on, monday, tuesday, wed, thurs, fri, sat, sun

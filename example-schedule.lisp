;;  DO times-alist and init state
(load "ajsm.lisp") ;this way you can just load this file and all will
		   ;be well

(in-package :ajsm)

(defvar *example-vertices* nil)
(defvar *example-graph* nil)
(defvar *example-init-state* nil)
(defvar *example-times-alist* nil)
(defvar *example-day-groups* nil)

(setf *example-vertices*
      '(
        ;; days
        (mon_fri nil 'days)
        (sat nil 'days)
        (mon nil 'days)
        (tues_sat nil 'days)
        (sun nil 'days)
        (wed nil 'days)
        (mon_sat nil 'days)
        ;; times
        (t0005 nil 'time-of-day)
        (t0030 nil 'time-of-day)
        (t0100 nil 'time-of-day)
        (t0200 nil 'time-of-day)
        (t0230 nil 'time-of-day)
        (t0330 nil 'time-of-day)
        (t0400 nil 'time-of-day)
        (t0405 nil 'time-of-day)
        (t0600 nil 'time-of-day)
        (t0650 nil 'time-of-day)
        (t1000 nil 'time-of-day)
        (t1100 nil 'time-of-day)
        (t1200 nil 'time-of-day)
        (t1600 nil 'time-of-day)
        (t1645 nil 'time-of-day)
        (t1700 nil 'time-of-day)
        (t1800 nil 'time-of-day)
        (t1807 nil 'time-of-day)
        (t1830 nil 'time-of-day)
        (t1900 nil 'time-of-day)
        (t1930 nil 'time-of-day)
        (t2100 nil 'time-of-day)
        (t2101 nil 'time-of-day)
        (t2145 nil 'time-of-day)
        (t2300 nil 'time-of-day)
        ;; devices
        (tapeu4 nil 'dev)
        ;; calendars
        (nonworking nil 'hcal)
        (example_dly nil 'scal)
        (monthstr nil 'scal) ;not really a calendar, but a logical
			     ;value in the IBM scheduler

        ;; groups
        ;; vertices and edges
        (job1 (t0100))
        (job2 (t1200 mon))
        (job3 (t1200 mon_fri))
        (job4 (t1900))
        (foojob (t1830))
        (ebs_up (t0650))
        (catsneeze (t1800 mon_fri))
        (foo_down (t2101 mon_fri))
        (foo_down2 (t1600 sat))
        (foo_down3 (t2300 mon_fri))
        (foo_start (t2145 mon_fri))
        (foo_start2 (t1700 sat))
        (foo_start3 (t0005 tues_sat))
        (barjob (t0400))
        (bazjob (t0405))
        (quxjob (t0200))
        (quuxjob (t1645 example_dly nonworking))
        (dailysav (example_dly nonworking t2100 mon_sat))
        (savall (t1930))
        (catd (t1800 example_dly nonworking))
        (catu (t0600 example_dly nonworking))
        (dlybrgdwn (dailysav tapeu4))
        (dogr (t0030))
        (dogs (sun t0330))
        (flabberg (monthstr t0230))
        (flabberm (t1930))
        (fluzptf (wed t1100))
        (fluztst (wed t1000))
        (snizzl (t1807))
        ))

        (setf *example-init-state* 
         '(
           ;; sunday slmnu1603 		;strange deps
           ;; m_f m_sa m_su			;days
           ;; sat_tapeuf dailybrms_tapeu4 itemupd_tapeu4 transsav_tapeu4 ;tapes
           ;; nonworking example_dly
           ))					     ;calendars

        (setf *example-times-alist* 
              '((t0005 . 0005)
                (t0030 . 0030)
                (t0100 . 0100)
                (t0200 . 0200)
                (t0230 . 0230)
                (t0330 . 0330)
                (t0400 . 0400)
                (t0405 . 0405)
                (t0600 . 0600)
                (t0650 . 0650)
                (t1000 . 1000)
                (t1100 . 1100)
                (t1200 . 1200)
                (t1600 . 1600)
                (t1645 . 1645)
                (t1700 . 1700)
                (t1800 . 1800)
                (t1807 . 1807)
                (t1830 . 1830)
                (t1900 . 1900)
                (t1930 . 1930)
                (t2100 . 2100)
                (t2101 . 2101)
                (t2145 . 2145)
                (t2300 . 2300)))

        (setf *example-graph* (ajsm:bulk-add-vertices *graph* *example-vertices*))

        ;; Day groups
        (setf *example-day-groups*
         '((mon_fri . (monday tuesday wednesday thursday friday))
           (mon_sat . (monday tuesday wednesday thursday friday saturday))
           (tues_sat . (tuesday wednesday thursday friday saturday))
           (sat . (saturday))
           (mon . (monday))
           (wed . (wednesday))
           (sun . (sunday))
           ))

(defun example-do-december-2012 ()
  (let ((graph *example-graph*)
        (times *example-times-alist*)
        (grps *example-day-groups*))
    (format t "~%Restting graph.~%")
    (reset-graph graph)
    (format t "Initializing state...~%")
    (loop for vertex in '(tapeu4 example_dly nonworking) do
         (run-vertex graph vertex))
    ;; week
    (loop for day in '(
                       ('saturday . 1)
                       ('sunday . 2)
                       ('monday . 3)
                       ('tuesday . 4)
                       ('wednesday . 5)
                       ('thursday . 6)
                       ('friday . 7)
                       ('saturday . 8)
                       ('sunday . 9)
                       ('monday . 10)
                       ('tuesday . 11)
                       ('wednesday . 12)
                       ('thursday . 13)
                       ('friday . 14)
                       ('saturday . 15)
                       ('sunday . 16)
                       ('monday . 17)
                       ('tuesday . 18)
                       ('wednesday . 19)
                       ('thursday . 20)
                       ('friday . 21)
                       ('saturday . 22)
                       ('sunday . 23)
                       ('monday . 24)
                       ('tuesday . 25)
                       ('wednesday . 26)
                       ('thursday . 27)
                       ('friday . 28)
                       ('saturday . 29)
                       ('sunday . 30)
                       )
       do
         (format t "~%~%~%::::: DOING ~a, DECEMBER ~a :::::~%~%" (car day) (cdr day))
         (if (= (cdr day) 1) (run-vertex graph 'monthstr))
         (if (or (equal (car day) ''sunday) (= (cdr day) 25))
             (progn  (unrun-vertex graph 'example_dly) (unrun-vertex graph 'nonworking)))
         ;; (format t "~a, ~a, ~a, ~a" graph (car day) grps times)
         (do-day graph (car day) grps times)
         (if (= (cdr day) 1) (unrun-vertex graph 'monthstr))
         (if (or (equal (car day) ''sunday) (= (cdr day) 25))
             (progn  (run-vertex graph 'example_dly) (run-vertex graph 'nonworking)))
         )))

 

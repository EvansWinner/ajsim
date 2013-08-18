;; For Emacs

;; Use these to interactively add nodes and edges to the job graph.
;; It interactively queries for job names in the minibuffer and
;; creates declarations like these:

;; (node ((id "MORNING") (label "MORNING")))
;; (node ((id "AFTRMORN") (label "AFTRMORN")))
;; (edge ((from "AFTRMORN") (to "MORNING")))

(defun add-node (node)
  (interactive "MNode name: ")
  (insert
   (concat
    "(node ((id \"" node "\") (label \"" node "\")))")))

(defun add-edge (from)
  (interactive "MFrom: ")
  (let ((to (read-from-minibuffer "To: ")))
    (insert
     (concat
      "(edge ((from \"" from "\") (to \"" to "\")))"))))

;; Uncomment these after deciding what key you want to bind.  The
;; bindings here are probably not a good idea -- surely they conflict
;; with something or other.

;; (define-key global-map "\C-cn" 'add-node)
;; (define-key global-map "\C-ce" 'add-edge)


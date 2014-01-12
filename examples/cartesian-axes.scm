;;;
;;; Example: Sine 2
;;; ===============
;;;
;;; This example demonstrates how to use Abscissa to generate a 2D line
;;; plot of a sine function with customization of axes labels, ranges, and
;;; grid lines.
;;;
;;; To run this example, either execute this as a script from the command line,
;;;
;;;     csi simple.scm
;;;
;;; or load it in the REPL,
;;;
;;;     (load "simple.scm")
;;;
;;; Both methods should result in the creation of an interactive window
;;; containing a single sine wave.

;; Import the abscissa module
(use abscissa)

;; Declare samples
(define x (linspace -3.2 3.2 0.01))

;; Declare a PDF named sine-2.pdf containging a single figure with
;; cartesian axes containing a single line plot of the sine function.  The
;; axes limits are declared to be [3.5,3.5] X [-1.5,1.5]. The axes
;; coordinate labels are declared to be "x" and "y". The axes major grid
;; lines are declared to be solid lines.
([pdf name: "sine-2.pdf"] 
 ([figure] 
  ([cartesian x-label: "x"
			  y-label: "y"
			  x-limits: '(-3.5 . 3.5)
			  y-limits: '(-1.5 . 1.5)
			  major-grid: '- ]
   ([lines] (<-sample- sin x)))))

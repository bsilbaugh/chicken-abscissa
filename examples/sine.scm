;;;
;;; Example: Sine
;;; ===============
;;;
;;; This example demonstrates how to use Abscissa to generate a basic 2D
;;; line plot of a sine function, and export it to a PDF file.
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

;; Declare a window containing a single figure, with Cartesian axes, and a
;; curve formed by patching together line segments ("lines") whose end points
;; coincide with the points defined by the sample set.
([pdf name: "sine.pdf"] 
 ([figure] 
  ([cartesian] 
   ([lines] (<-sample- sin (linspace -3.14 3.14 0.01))))))



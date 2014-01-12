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

;; Set the name of the pdf output file.  We need to "extend the plotting
;; language" by creating a custom pdf object, as the default pdf-file
;; object will use a default file name of "abscissa-plot.pdf". (This would
;; still work, but we want to have better control over the file name for
;; this example.) We'll do this by using the meta-pdf-file object.
(define pdf (meta-pdf-file name: "sine.pdf"))

;; Declare a window containing a single figure, with Cartesian axes, and a
;; curve formed by patching together line segments ("lines") whose end points
;; coincide with the points defined by the sample set.
(pdf (figure (cartesian (lines (<-sample- sin (linspace -3.14 3.14 0.01))))))



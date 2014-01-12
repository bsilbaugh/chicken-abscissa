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

;; Set the name of the pdf output file.  We need to "extend the plotting
;; language" by creating a custom pdf object, as the default pdf-file
;; object will use a default file name of "abscissa-plot.pdf". (This would
;; still work, but we want to have better control over the file name for
;; this example.) We'll do this by using the meta-pdf-file object.
(define pdf (meta-pdf-file name: "sine-2.pdf"))

;; Create custom cartesian axes object with x and y limits set to
;; [-3.15,3.15] and [-1,1], respectively, axes x and y labeld "x" and "y",
;; respectively, and solid major grid lines.
(define axes (meta-cartesian x-limits: '(-3.15 . 3.15)
							 y-limits: '(-1.0 . 1.0)
							 x-label: "x"
							 y-label: "y"
							 major-grid: '-))

;; Define sample sites
(define x (linspace -3.2 3.2 0.01))

;; Declare a window containing a single figure, with Cartesian axes, and a
;; curve formed by patching together line segments ("lines") whose end points
;; coincide with the points defined by the sample set.
(pdf (figure (axes (lines (<-sample- sin x)))))

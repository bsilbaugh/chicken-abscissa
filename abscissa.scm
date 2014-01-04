;;;
;;; Abscissa
;;;
;;; An embedded domain specific language for plotting 2D datasets in
;;; Chicken Scheme.
;;;
;;; Copyright 2014 Benjamin Silbaugh

(use posix)

(define (interactive win)
  (call-with-output-pipe "gnuplot -persist" win))

(define (batch file)
  (call-with-output-pipe "gnuplot" file))

(define (window fig)
  (lambda (p) (fig p)))

(define (pdf-file fig)
  (lambda (p)
	(display "set term pdf" p)
	(newline p)
	(display "set output \"test.pdf\" " p)
	(newline p)
	(fig p)))

(define (figure ax)
  (lambda (p) (ax p)))

(define (axes . cases)
  (lambda (p)
	(display "set grid" p)
	(newline p)
	(display "set xlabel \"X\"" p)
	(newline p)
	(display "set ylabel \"Y\"" p)
	(newline p)
	(for-each (lambda (f) (f p)) cases)))

(define (lines data-set)
  (lambda (p)
	(display "plot '-' with lines" p)
	(newline p)
	(data-set p)))

(define (linespoints data-set)
  (lambda (p)
	(display "plot '-' with linespoints" p)
	(newline p)
	(data-set p)))

(define (xy-pairs pairs)
  (lambda (p)
	(letrec ((display-pair
			  (lambda (xy)
				(display (car xy) p)
				(display " " p)
				(display (cdr xy) p)
				(newline p))))
	  (for-each display-pair pairs)
	  (display "e" p)
	  (newline p))))

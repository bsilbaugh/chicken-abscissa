;;;
;;; Abscissa
;;;
;;; An embedded domain specific language for plotting 2D datasets in
;;; Chicken Scheme.
;;;
;;; Copyright 2014 Benjamin Silbaugh
;;;
;;; See LICENSE file for modification and redistribution permissions.

(module abscissa 
  (meta-window
   meta-pdf-file
   meta-cartesian
   meta-lines
   meta-points
   meta-lines-points
   window 
   pdf-file
   figure
   cartesian
   lines
   points
   lines-points
   data-pairs)

(import scheme chicken)
(use data-structures)
(use posix)
(use srfi-1)

;;; === Colors ===

(define *red*     "dark-red") 
(define *blue*    "dark-blue")
(define *green*   "dark-green")
(define *magenta* "#500050")
(define *cyan*    "#006969")
(define *yellow*  "dark-yellow")

;;; === helpers ===

(define (interactive win)
  (call-with-output-pipe "gnuplot -persist" win))

(define (batch file)
  (call-with-output-pipe "gnuplot" file))

;;; === Plot "Meta" Elements ===

(define ((meta-window) fig)
  (interactive
   (lambda (p) (fig p))))

(define ((meta-pdf-file #!key 
						(width 5.0) 
						(height 3.0) 
						(file-name "abscissa-plot.pdf")) fig)
  (define (display-size p)
	(display "size " p)
	(display width p)
	(display ", " p)
	(display height p)
	(display " " p))
  (define (display-file-name p)
	(display #\" p)
	(display file-name p)
	(display #\" p)
	(display " " p))
  (batch
   (lambda (p)
	 (display "set term pdf " p)
	 (display-size p)
	 (newline p)
	 (display "set output " p)
	 (display-file-name p)
	 (newline p)
	 (fig p))))

(define ((meta-figure) ax)
  (lambda (p) (ax p)))

(define ((meta-cartesian #!key 
						 (x-label #f)
						 (y-label #f)
						 (major-grid #f)) . cases)
  (define (display-comma p)
	(display ", " p))
  (define (display-label cmd-str label p)
	(if label
		(begin
		  (display cmd-str p)
		  (display #\" p)
		  (display label p)
		  (display #\" p)
		  (newline p))))
  (define (display-x-label p)
	(display-label "set xlabel " x-label p))
  (define (display-y-label p)
	(display-label "set ylabel " y-label p))
  (define (display-grid p)
	(cond ((eq? '-- major-grid)
		   (display "set grid linestyle 0" p))
		  ((eq? '- major-grid)
		   (display "set grid linestyle -1 lc rgb \"grey\"" p))
		  ((fixnum? major-grid)
		   (display "set grid linestyle " p)
		   (display (number->string major-grid) p)))
	(newline p))
  (let ((with-stmts (intersperse (map car cases) display-comma))
		(data-stmts (map cdr cases)))
	(lambda (p)
	  (display-grid p)
	  (display-x-label p)
	  (display-y-label p)
	  (display "plot " p)
	  (for-each (lambda (f) (f p)) with-stmts)
	  (newline p)
	  (for-each (lambda (f) (f p)) data-stmts))))

; TODO
; (define ((meta-polar-axes) . cases))

(define ((meta-lines #!key 
					 (style '-) 
					 (color "dark-blue") 
					 (weight 1)) data-set)
  (define (line-style s)
	(cond ((eq? '-  style) -1) ; solid line
		  ((eq? '-- style) 0) ; dashed line
		  ((and (fixnum? style) (< 0 style)) style)
		  (error "Unrecognized line style value: " style)))
  (define (display-style p)
	(display "linestyle " p)
	(display (line-style style) p)
	(display #\space p))
  (define (display-color p)
	(display "lc rgb " p)
	(display #\" p)
	(display color p)
	(display #\" p)
	(display #\space p))
  (define (display-weight p)
	(display "lw " p)
	(display weight p)
	(display " " p))
  (define (with-stmt p)
	(display "'-' with lines " p)
	(display-style p)
	(display-color p)
	(display-weight p))
  (cons with-stmt data-set))

(define ((meta-points #!key 
					  (style 'o) 
					  (color "dark-blue") 
					  (weight 1)) data-set)
  (define (line-type style)
	(cond ((eq? 'o style) 32)
		  ((eq? '+ style) 27)
		  ((eq? 'x style) 28)
		  ((eq? 's style) 30)
		  ((eq? 'd style) 25)
		  ((eq? 't style) 34)))
  (define (display-style p)
	(display "linetype " p)
	(display (line-type style) p)
	(display #\space p))
  (define (display-color p)
	(display "lc rgb " p)
	(display #\" p)
	(display color p)
	(display #\" p)
	(display #\space p))
  (define (display-weight p)
	(display "ps " p)
	(display weight p)
	(display #\space p))
  (define (with-stmt p)
	(display "'-' with points " p)
	(display-style p)
	(display-color p)
	(display-weight p))
  (cons with-stmt data-set))

(define ((meta-lines-points #!key 
							(style 'o-)
							(color *blue*) 
							(weight 1)) data-set)
  (define (line-type style)
	(cond ((or (eq? 'o- style)
			   (eq? '+- style)
			   (eq? 'x- style)
			   (eq? 's- style)
			   (eq? 'd- style)
			   (eq? 't- style)) -1)
		  ((or (eq? 'o-- style)
			   (eq? '+-- style)
			   (eq? 'x-- style)
			   (eq? 's-- style)
			   (eq? 'd-- style)
			   (eq? 't-- style)) 0)))
  (define (point-type style)
	(cond ((or (eq? 'o- style) (eq? 'o-- style)) 32)
		  ((or (eq? '+- style) (eq? '+-- style)) 27)
		  ((or (eq? 'x- style) (eq? 'x-- style)) 28)
		  ((or (eq? 's- style) (eq? 's-- style)) 30)
		  ((or (eq? 'd- style) (eq? 'd-- style)) 25)
		  ((or (eq? 't- style) (eq? 't-- style)) 34)))
  (define (display-lines-type p)
	(display "linetype " p)
	(display (line-type style) p)
	(display #\space p))
  (define (display-point-type p)
	(display "pointtype " p)
	(display (point-type style) p)
	(display #\space p))
  (define (display-color p)
	(display "lc rgb " p)
	(display #\" p)
	(display color p)
	(display #\" p))
  (define (display-weight p)
	(display "lw " p)
	(display weight p)
	(display #\space p))
  (define (with-stmt p)
	(display "'-' with linespoints " p)
	(display-lines-type p)
	(display-point-type p)
	(display-color p)
	(display-weight p))
  (cons with-stmt data-set))

;;; === Plot Primitives ===

(define *default-color-seq*
  (list *red*
		*blue*
		*green*
		*magenta*
		*cyan*
		*yellow*))

(define *default-lines-clist*
  (apply circular-list 
		 (map (lambda (c) (meta-lines color: c)) *default-color-seq*)))

(define *default-points-clist*
  (apply circular-list
		 (map (lambda (c ps) (meta-points style: ps color: c)) 
			  *default-color-seq*
			  '(o + x s d t))))

(define *default-lines-points-clist*
  (apply circular-list
		 (map (lambda (c ps) (meta-lines-points style: ps color: c))
			  *default-color-seq*
			  '(o- +- x- s- d- t-))))

(define-syntax define-cyclic
  (er-macro-transformer
   (lambda (exp rename compare)
	 (let ((name (cadr exp))
		   (fun-seq (caddr exp)))
	   `(define ,name (lambda args
						(let ((x (car ,fun-seq)))
						  (set! ,fun-seq (cdr ,fun-seq))
						  (apply x args))))))))

(define window (meta-window))

(define pdf-file (meta-pdf-file))

(define figure (meta-figure))

(define cartesian (meta-cartesian))

(define-cyclic lines *default-lines-clist*)

(define-cyclic points *default-points-clist*)

(define-cyclic lines-points *default-lines-points-clist*)

(define (data-pairs pairs)
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

;;; === High Level Convenience Functions ===

) ; end module

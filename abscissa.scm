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
   meta-figure
   meta-cartesian
   meta-lines
   meta-points
   meta-lines-points
   <-meta
   <-meta-zip-
   window 
   pdf-file
   figure
   cartesian
   lines
   points
   <-
   <-zip-
   xy-line-plot
   xy-scatter-plot)

(import scheme chicken)
(use data-structures)
(use posix)
(use srfi-1)

;;; === Colors ===

(define *black*   "black")
(define *grey*    "grey")
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

(define ((meta-figure #!key (show-legend #f)) ax)
  (define (display-legend p)
	(if show-legend
		(begin
		  (display "set key rmargin top vertical Left reverse" p)
		  (newline p))
		(begin
		  (display "unset key" p)
		  (newline p))))
  (lambda (p)
	(display-legend p)
	(ax p)))

(define ((meta-cartesian #!key 
						 (x-limits #f)
						 (y-limits #f)
						 (x-label #f)
						 (y-label #f)
						 (major-grid #f)) . cases)
  (define (display-comma p)
	(display ", " p))
  (define (display-limits cmd-str ab p)
	(if ab
		(begin
		  (display cmd-str p)
		  (display "[" p)
		  (display (car ab) p)
		  (display ":" p)
		  (display (cdr ab) p)
		  (display "]" p)
		  (newline p))))
  (define (display-label cmd-str label p)
	(if label
		(begin
		  (display cmd-str p)
		  (display #\" p)
		  (display label p)
		  (display #\" p)
		  (newline p))))
  (define (display-x-limits p)
	(display-limits "set xrange " x-limits p))
  (define (display-y-limits p)
	(display-limits "set yrange " y-limits p))
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
	  (display-x-limits p)
	  (display-y-limits p)
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
					 (color *blue*) 
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
	(display "with lines " p)
	(display-style p)
	(display-color p)
	(display-weight p))
  (define title-stmt (car data-set))
  (define display-data (cdr data-set))
  (define (display-cmds p)
	(display "'-' " p)
	(title-stmt p)
	(with-stmt p))
  (cons display-cmds display-data))

(define ((meta-points #!key 
					  (style 'o) 
					  (color *blue*) 
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
	(display "with points " p)
	(display-style p)
	(display-color p)
	(display-weight p))
  (define title-stmt (car data-set))
  (define display-data (cdr data-set))
  (define (display-cmds p)
	(display "'-' " p)
	(title-stmt p)
	(with-stmt p))
  (cons display-cmds display-data))

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
	(display "with linespoints " p)
	(display-lines-type p)
	(display-point-type p)
	(display-color p)
	(display-weight p))
  (define title-stmt (car data-set))
  (define display-data (cdr data-set))
  (define (display-cmds p)
	(display "'-' " p)
	(title-stmt p)
	(with-stmt p))
  (cons display-cmds display-data))

(define (<-meta-data display-data label)
  (define (display-title p)
	(display "title " p)
	(display #\" p)
	(display label p)
	(display #\" p)
	(display #\space p))
   (cons display-title display-data))

(define ((<-meta #!key (label "NONE")) pairs)
   (define (display-data p)
	 (letrec ((display-pair
			   (lambda (xy)
				 (display (car xy) p)
				 (display " " p)
				 (display (cdr xy) p)
				 (newline p))))
	   (for-each display-pair pairs)
	   (display "e" p)
	   (newline p)))
   (<-meta-data display-data label))

(define ((<-meta-zip- #!key (label "NONE")) x y)
  (define (display-data p)
	(for-each (lambda (xi yi)
				(display xi p)
				(display #\space p)
				(display yi p)
				(newline p)) x y)
	(display "e" p)
	(newline p))
  (<-meta-data display-data label))

;;; === Plot Elements ===

(define window (meta-window))

(define pdf-file (meta-pdf-file))

(define figure (meta-figure))

(define cartesian (meta-cartesian))

(define lines (meta-lines))

(define points (meta-points))

(define <- (<-meta))

(define <-zip- (<-meta-zip-))

;;; === High Level Convenience Functions ===

;; helper function for building various style xy-plots
(define (xy-plot style-seq xy-pairs)
  (define (labels xy-pairs i)
	(if (null? xy-pairs)
		'()
		(cons (string-append "case " (number->string i))
			  (labels (cdr xy-pairs) (+ i 1)))))
  (window
   ((meta-figure show-legend: #t)
	(apply (meta-cartesian major-grid: '--)
		   (map (lambda (style label xy)
				  (style ((<-meta label: label) xy)))
				style-seq (labels xy-pairs 1) xy-pairs)))))

;; Useful for generating quick-n-dirty line plots of xy data
(define (xy-line-plot #!rest xy-pairs)
  (xy-plot (circular-list 
			(meta-lines color: *red*)
			(meta-lines color: *blue*)
			(meta-lines color: *green*)
			(meta-lines color: *cyan*)
			(meta-lines color: *magenta*))
		   xy-pairs))

;; Useful for generating quick-n-dirty scatter plots of xy data
(define (xy-scatter-plot #!rest xy-pairs)
  (xy-plot (circular-list
			(meta-points style: 'o color: *red*)
			(meta-points style: '+ color: *blue*)
			(meta-points style: 'x color: *green*)
			(meta-points style: 's color: *cyan*)
			(meta-points style: 't color: *magenta*)
			(meta-points style: 'd color: *yellow*))
		   xy-pairs))

) ; end module

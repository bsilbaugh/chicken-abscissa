;;;
;;; Example: Legend
;;;
;;; Demonstrate usages of a plot legend (key).

(use abscissa)

(define pi (* 4.0 (atan 1.0)))
(define x (linspace -1.0 1.0 0.001))
(define ((phase n) x) 
  (cos (* 2 pi (- x n))))

([pdf name: "legend.pdf"]
 ([figure show-legend: #t]
  ([cartesian]
   ([lines color: "dark-red"     label: "1st"] (<-sample- (phase 0.0) x))
   ([lines color: "dark-blue"    label: "2nd"] (<-sample- (phase 0.1) x))
   ([lines color: "dark-green"   label: "3rd"] (<-sample- (phase 0.2) x))
   ([lines color: "dark-cyan"    label: "4th"] (<-sample- (phase 0.3) x))
   ([lines color: "dark-magenta" label: "5th"] (<-sample- (phase 0.4) x))
   ([lines color: "dark-yellow"  label: "6th"] (<-sample- (phase 0.5) x)))))

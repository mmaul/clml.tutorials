
(progn
    (ql:quickload '(:clml.utility ; Need clml.utility.data to get data from the net
                :clml.hjs ; Need clml.hjs.read-data to poke around the raw dataset
                :clml.pca
                :clml.clustering
                :iolib
                :clml.extras.eazy-gnuplot
                :eazy-gnuplot
                ))
    (defpackage #:wine
      (:use #:cl
        #:cl-jupyter-user
        #:clml.hjs.read-data
        #:clml.utility.data
        #:clml.hjs.vector
        #:clml.hjs.matrix
        #:clml.hjs.k-means
        #:clml.pca
        #:clml.clustering.hc
        #:eazy-gnuplot
        ))
)

(in-package :wine)

(defparameter uci-wine 
    (read-data-from-file
        (fetch "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
        :type :csv))

(format nil "~A" (head-points uci-wine))

(let ((wine-unspecialized (read-data-from-file
    (fetch "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
    :type :csv
    :csv-type-spec '(integer
                     double-float double-float  double-float
                     double-float double-float  double-float
                     double-float double-float  double-float
                     double-float double-float  double-float
                     double-float)
    :csv-header-p ( list "Class"
                         "Alcohol"            "Malic acid"           "Ash"
                         "Alcalinity of ash"  "Magnesium"            "Total phenols"
                         "Flavanoids"         "Nonflavanoid phenols" "Proanthocyanins"
                         "Color intensity"    "Hue"                   "OD280/OD315 of diluted wines"
                         "Proline")
    )))
  (defparameter wine
    (pick-and-specialize-data 
       wine-unspecialized   
       :range '(1 2 3 4 5 6 7 8 9 10 11 12 13) :data-types (make-list 13 :initial-element :numeric )))
  (defparameter wine-with-classifications
    (pick-and-specialize-data 
       wine-unspecialized   
       :data-types (make-list 14 :initial-element :numeric )))
       
  (defparameter wine-classifications (loop for r across (dataset-points wine-with-classifications) 
                                           when (= (elt r 0) 1d0) count r into one 
                                           when (= (elt r 0) 2d0) count r into two 
                                           when (= (elt r 0) 3d0) count r into three 
                                           finally (return (list (list 1 one) (list 2 two) (list 3 three)))))

)

(progn 
  (defparameter standardized-wine (copy-dataset wine))
  (setf (dataset-numeric-points standardized-wine) (standardize (dataset-numeric-points standardized-wine)))
  
  (defparameter pca-result (princomp standardized-wine))
)

(let ((png "wine-pca-contributions.png"))
    (clml.extras.eazy-gnuplot::plot-series (contributions pca-result) 
    :term '(:png) :output png :plot-title "PCA Contributions" :ylabel "Variance" :xlabel "Column" :series-title "")
    (display-png (png-from-file png)))

(defparameter truncated-standardized-wine (make-numeric-dataset  
                       (map 'list #'dimension-name (subseq (dataset-dimensions wine) 0 4))                           
                       (map 'vector (lambda (r) (subseq r 0 4)) (dataset-points standardized-wine))))

(let ((wss1 (* (- (length (components pca-result)) 1) 
               (loop for v across (subseq (contributions pca-result) 0 4) sum v)))
       (comp-ds (make-numeric-dataset '("pc1" "pc2" "pc3" "pc4")
                                      (map 'vector (lambda (r) (subseq r 0 4)) (components pca-result))))
        (png "group-sum-of-squares.png"))
    (clml.extras.eazy-gnuplot::plot-series 
      (coerce (cons wss1 
                  (loop for n from 2 upto 8 ; could be up size of dimensions the far end is generally irrelevant
                        for k-means-n = (k-means n comp-ds  :standardization nil 
                                                 :random-state (make-random-state-with-seed 100))
                        collect (loop for x across (clml.hjs.k-means::pw-distance-between-point-and-owner k-means-n) 
                                      sum (* x x)))) 
               'vector)
       :term '(:png) :output png :plot-title "Group Sum of Squares" :ylabel "In group sum of squares" 
       :xlabel "Clusters" :series-title "")
  
  (display-png (png-from-file png)))

 (progn  (defparameter workspace nil) 
         (defparameter table nil)
         (multiple-value-setq (workspace table)
           (k-means 3 truncated-standardized-wine  :standardization nil 
                    :random-state (make-random-state-with-seed 1234))))

(loop for c across (!! wine-with-classifications "Class") 
  when (= c 1.0) count c into one when (= c 2.0) count c into two when (= c 3.0) count c into three 
  finally (return (list (list 1  one) (list 2 two) (list 3 three))))

wine-classifications

(progn
  (defparameter distance-matrix (distance-matrix (numeric-matrix standardized-wine)))
  (defparameter u nil) (defparameter v nil)
  (multiple-value-setq (u v) (cophenetic-matrix distance-matrix #'hc-ward))
  (defparameter ctree (cutree 3 v))
  (format t "Cut tree: ~A ~%Class counts:~A ~%" ctree
           (loop for x across ctree 
                 when (= x 1) counting x into one 
                 when (= x 2) counting x into two 
                 when (= x 3) counting x into three 
                 finally (return (list (list 1 one) (list 2 two) (list 3 three)))))
)

wine-classifications


(ql:quickload '(:clml.utility ; Need clml.utility.data to get data from the net
                :clml.hjs ; Need clml.hjs.read-data for dataset
                :iolib
                :clml.extras.eazy-gnuplot
                :eazy-gnuplot
            ))

(defpackage #:datasets-tutorial
  (:use #:cl
        #:cl-jupyter-user ; Not needed unless using iPython notebook
        #:clml.hjs.read-data
        #:clml.hjs.meta ; util function
        #:clml.extras.eazy-gnuplot))


(in-package :datasets-tutorial)

(defparameter dataset (read-data-from-file 
                       (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/cars.csv") 
                       :type :csv :csv-type-spec '(integer integer)))

(dataset-points dataset)

(head-points dataset)

(dataset-dimensions dataset)

(make-numeric-and-category-dataset  
 '("cat 1" "num 1")                           ; <-- Column names 
 (vector (v2dvec #(1.0d0)) (v2dvec #(2.0d0))) ; <-- Numeric data 
          '(1)                                ; <-- Indexes of numeric column
          #(#("a") #("b"))                    ; <-- Category Data
          '(0)                                ; <-- Indexes of category data
)

(pick-and-specialize-data dataset :data-types '(:numeric :numeric))

(let ((ds (pick-and-specialize-data dataset :data-types '(:numeric :numeric) 
           :store-numeric-data-as-matrix t)))
           (print ds)
           (dataset-numeric-points ds))

(pick-and-specialize-data (read-data-from-file 
                           (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/UKgas.sexp"))
     :data-types '(:category :numeric))

(let ((ds (read-data-from-file 
            (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/simple1.csv") 
            :type :csv 
            :csv-type-spec '(double-float double-float string) 
            :missing-values-list '("NA")
          )))
            (format nil "~A~%~A~%" ds (dataset-points ds)))

(let ((ds (pick-and-specialize-data
             (read-data-from-file 
               (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/simple1.csv") 
               :type :csv 
               :csv-type-spec '(double-float double-float string) 
               :missing-values-list '("NA")
              )
              :data-types '(:numeric :numeric :category)
           )))
     (format nil "~A~%~A~%~A~%" ds  (dataset-numeric-points ds) (dataset-category-points ds))
)

(defparameter ukgas (pick-and-specialize-data (read-data-from-file 
                           (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/UKgas.sexp"))
                           :data-types '(:category :numeric)))

(copy-dataset ukgas)

 (multiple-value-list (divide-dataset ukgas :divide-ratio '(3 1) :random t))

(make-bootstrap-sample-datasets ukgas :number-of-datasets 3)

 (dataset-cleaning ukgas :outlier-types-alist '(("UKgas" . :std-dev)) 
                         :outlier-values-alist '((:std-dev . 1)) 
                         :interp-types-alist '(("UKgas" . :zero)))

(add-dim ukgas "mpg" :numeric :initial-value 0.0d0)

(concatenate-datasets ukgas ukgas)

 (write-dataset ukgas "gasgas.csv")

(!! ukgas "UKgas")

(dataset-numeric-points ukgas)

(ql:quickload :clml.r-datasets)

(use-package :clml.r-datasets)

(defparameter dd (get-r-dataset-directory))

(subseq (inventory dd :stream nil) 0 505)

(subseq (dataset-documentation  dd  "datasets" "BOD" :stream nil) 0 200)

(defparameter bod (get-dataset dd "datasets" "BOD" :csv-type-spec '(double-float double-float double-float)))

bod

 (pick-and-specialize-data bod :data-types '(:numeric :numeric :numeric))

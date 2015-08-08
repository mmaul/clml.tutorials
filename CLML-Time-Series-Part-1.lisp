
(ql:quickload '(:clml.utility ; Need clml.utility.data to get data from the net
                :clml.hjs ; Need clml.hjs.read-data to poke around the raw dataset
                :clml.time-series ; Need Time Series package obviously
                :iolib
                :clml.extras.eazy-gnuplot
                :eazy-gnuplot
            ))

(defpackage #:time-series-part-1
  (:use #:cl
        #:fishbowl-user ; Not needed unless using iPython notebook
        #:clml.time-series.read-data
        #:clml.time-series.anomaly-detection
        #:clml.time-series.exponential-smoothing
        #:clml.extras.eazy-gnuplot)
  (:import-from #:clml.hjs.read-data #:head-points #:!! #:dataset-dimensions)
  (:import-from #:clml.time-series.util #:predict)
  (:import-from #:clml.hjs.read-data #:read-data-from-file)
  )


(in-package :time-series-part-1)

(defparameter dataset (read-data-from-file 
        (clml.utility.data:fetch 
            "https://mmaul.github.io/clml.data/sample/msi-access-stat/access-log-stat.sexp")))

dataset

(head-points dataset)

(defparameter msi-access (time-series-data dataset :range '(1) :time-label 0 :frequency 24 :start '(18 3)))

msi-access

(subseq(ts-points msi-access) 0 5)

(svg 
    (plot-dataset msi-access "hits" :terminal '(:svg :enhanced :mouse :size 900 600)
        :range '(0 40) :title "MSI Access Log - first 40 points" :ytics-font ",8" :xtics-font ",8"
        :xlabel-font ",15" :ylabel-font ",15"))

(svg (plot-dataset msi-access "hits" :terminal '(:svg :enhanced :mouse :size 900 600)
        :title "MSI Access Log" :ytics-font ",8" :xtics-font ",8"
        :xlabel-font ",15" :ylabel-font ",15" :xtic-interval 500)))

(defparameter c-msi-access 
    (ts-cleaning msi-access :outlier-types-alist '(("hits" . :std-dev)) 
                            :outlier-values-alist '((:std-dev . 5)) 
                            :interp-types-alist '(("hits" . :mean))))

(svg (plot-dataset c-msi-access "hits" :terminal '(:svg :enhanced :mouse :size 900 600)

        :title "Cleaned MSI Access Log" :ytics-font ",8" :xtics-font ",8"

        :xlabel-font ",15" :ylabel-font ",15" :xtic-interval 500
        :yrange '(0 8000)))



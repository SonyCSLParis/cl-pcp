;; Copyright 2021-present Sony Computer Science Laboratories Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                ;;
;; Demo file for the cl-pcp system for parallel corpus processing in common lisp. ;;
;;                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We start by loading the cl-pcp system and entering its package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :cl-pcp)
(in-package :cl-pcp)

;; Let us first generate some test data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *benchmark-data-input-file* (make-pathname :directory  (temp-directory) :name "benchmark-data-in" :type "txt"))

(defun generate-test-data (file min-load max-load nr-of-loads)
  "Generates benchmark data: file with nr-of-loads lines holding a random nr between min-load and max-load."
  (let ((benchmark-data-file file))
    (with-open-file (stream benchmark-data-file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop with random-range = (- max-load min-load)
            for i from 1 upto nr-of-loads
            for random-nr = (+ min-load (random random-range))
            do (format stream "~a~%" random-nr)
            sum random-nr))))

;; (return value is total sum of numbers on all lines)
(generate-test-data *benchmark-data-input-file* 1 5 100)

;; Let us now process every line in the test data file using a computation-intensive function (see benchmark.lisp) ;;
;; and write the same line to the output-file.                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Outputfile
(defparameter *benchmark-data-output-file* (make-pathname :directory (temp-directory) :name "benchmark-data-out" :type "txt"))

;; Processing: time 
(time
 (process-corpus-in-parallel *benchmark-data-input-file* ;; input file with corpus
                             *benchmark-data-output-file* ;; output file for processed corpus
                             :external-lisp *external-lisp* ;; underlying lisp implementation to use (:lispworks :ccl :sbcl)
                             :asdf-systems nil ;; asdf-systems to are used (to be loaded by the client processes)
                             :read-fn 'read-from-stream ;; function to use for rading from stream connected to input file ('read-from-stream 'read-line-from-stream)
                             :process-fn 'cl-pcp::use-cpu ;; function to process each item of the corpus
                             :write-fn 'identity ;; transform the data resulting from process-fn to write away
                             :nr-of-processes 4  ;; number of subprocesses to use (do not exceed nr of cores on your machine)
                             :keep-order nil)) ;; Keep order of items in corpus 

(time
 (process-corpus-in-parallel *benchmark-data-input-file* ;; input file with corpus
                             *benchmark-data-output-file* ;; output file for processed corpus
                             :external-lisp *external-lisp* ;; underlying lisp implementation to use (:lispworks :ccl :sbcl)
                             :asdf-systems nil ;; asdf-systems to are used (to be loaded by the client processes)
                             :read-fn 'read-from-stream ;; function to use for rading from stream connected to input file ('read-from-stream 'read-line-from-stream)
                             :process-fn 'cl-pcp::use-cpu ;; function to process each item of the corpus
                             :write-fn 'identity ;; transform the data resulting from process-fn to write away
                             :nr-of-processes 8  ;; number of subprocesses to use (do not exceed nr of cores on your machine)
                             :keep-order nil)) ;; Keep order of items in corpus 

(time
 (process-corpus-in-parallel *benchmark-data-input-file* ;; input file with corpus
                             *benchmark-data-output-file* ;; output file for processed corpus
                             :external-lisp *external-lisp* ;; underlying lisp implementation to use (:lispworks :ccl :sbcl)
                             :asdf-systems nil ;; asdf-systems to are used (to be loaded by the client processes)
                             :read-fn 'read-from-stream ;; function to use for rading from stream connected to input file ('read-from-stream 'read-line-from-stream)
                             :process-fn 'cl-pcp::use-cpu ;; function to process each item of the corpus
                             :write-fn 'identity ;; transform the data resulting from process-fn to write away
                             :nr-of-processes 8  ;; number of subprocesses to use (do not exceed nr of cores on your machine)
                             :keep-order t)) ;; Keep order of items in corpus

(time
 (process-corpus-in-parallel *benchmark-data-input-file* ;; input file with corpus
                             *benchmark-data-output-file* ;; output file for processed corpus
                             :external-lisp *external-lisp* ;; underlying lisp implementation to use (:lispworks :ccl :sbcl)
                             :asdf-systems nil ;; asdf-systems to are used (to be loaded by the client processes)
                             :read-fn 'read-from-stream ;; function to use for rading from stream connected to input file ('read-from-stream 'read-line-from-stream)
                             :process-fn 'cl-pcp::use-cpu-with-kwargs ;; function to process each item of the corpus
                             :process-fn-kwargs '(:symbol-key foo :string-key "bar" :number-key 5 :list-key (a "a" 5))
                             :write-fn 'identity ;; transform the data resulting from process-fn to write away
                             :nr-of-processes 4  ;; number of subprocesses to use (do not exceed nr of cores on your machine)
                             :keep-order nil)) ;; Keep order of items in corpus 




;; Let us know look at an example where we use FCG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We start by loading our demo system that loads FCG and some specific fcg demo functions (in cl-pcp-fcg-demo/fcg-functions.lisp) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :cl-pcp-fcg-demo)


;; Let us generate some test data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fcg-benchmark-data-input-file* (make-pathname :directory  (cl-pcp::temp-directory) :name "fcg-benchmark-data-in" :type "txt"))
(defparameter *fcg-benchmark-data-output-file* (make-pathname :directory (cl-pcp::temp-directory) :name "fcg-benchmark-data-out" :type "txt"))


(defun generate-fcg-test-data (file)
  "Generates benchmark data for FCG's demo grammar."
  (let ((benchmark-data-file file))
    (with-open-file (stream benchmark-data-file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop for i from 1 to 100
        do (format stream "the linguist likes the mouse~%")))))

(generate-fcg-test-data *fcg-benchmark-data-input-file*)



;; Let us now call an FCG function on the 100 utterances of the test data that uses a timeout ;;
;; and write the resulting meaning representations as json to the output file                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(time
 (process-corpus-in-parallel *fcg-benchmark-data-input-file* ;; input file with corpus
                             *fcg-benchmark-data-output-file* ;; output file for processed corpus
                             :external-lisp *external-lisp* ;; underlying lisp implementation to use (:lispworks :ccl :sbcl)
                             :asdf-systems '(:cl-pcp-fcg-demo) ;; asdf-systems to are used (to be loaded by the client processes)
                             :read-fn 'read-line-from-stream ;; function to use for rading from stream connected to input file ('read-from-stream 'read-line-from-stream)
                             :process-fn 'fcg:comprehend-using-demo-grammar-with-timeout ;; function to process each item of the corpus
                             :write-fn 'fcg:encode-fcg-meaning-as-json ;; transform the data resulting from process-fn to write away
                             :nr-of-processes 4  ;; number of subprocesses to use (do not exceed nr of cores on your machine)
                             :keep-order nil))


(time
 (process-corpus-in-parallel *fcg-benchmark-data-input-file* ;; input file with corpus
                             *fcg-benchmark-data-output-file* ;; output file for processed corpus
                             :external-lisp *external-lisp* ;; underlying lisp implementation to use (:lispworks :ccl :sbcl)
                             :asdf-systems '(:cl-pcp-fcg-demo) ;; asdf-systems to are used (to be loaded by the client processes)
                             :read-fn 'read-line-from-stream ;; function to use for rading from stream connected to input file ('read-from-stream 'read-line-from-stream)
                             :process-fn 'fcg:comprehend-using-demo-grammar-with-timeout ;; function to process each item of the corpus
                             :write-fn 'fcg:encode-fcg-meaning-as-json ;; transform the data resulting from process-fn to write away
                             :nr-of-processes 8  ;; number of subprocesses to use (do not exceed nr of cores on your machine)
                             :keep-order nil))

(time
 (process-corpus-in-parallel *fcg-benchmark-data-input-file* ;; input file with corpus
                             *fcg-benchmark-data-output-file* ;; output file for processed corpus
                             :external-lisp *external-lisp* ;; underlying lisp implementation to use (:lispworks :ccl :sbcl)
                             :asdf-systems '(:cl-pcp-fcg-demo) ;; asdf-systems to are used (to be loaded by the client processes)
                             :read-fn 'read-line-from-stream ;; function to use for rading from stream connected to input file ('read-from-stream 'read-line-from-stream)
                             :process-fn 'fcg:comprehend-using-demo-grammar-with-timeout ;; function to process each item of the corpus
                             :write-fn 'fcg:encode-fcg-meaning-as-json ;; transform the data resulting from process-fn to write away
                             :nr-of-processes 8  ;; number of subprocesses to use (do not exceed nr of cores on your machine)
                             :keep-order t))

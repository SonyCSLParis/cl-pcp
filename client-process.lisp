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

(in-package :cl-pcp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; This file implements the client process side of the :cl-pcp package  ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *lock-file* nil
  "The name of the temporary lock file.")

(defparameter *output-file* nil
  "The name of the temporary output file.")

(defparameter *process-fn* nil
  "The name of the client process.")

(defparameter *process-fn-kwargs* nil
  "The keyword arguments of the client process.")

(defparameter *write-fn* nil
  "The name of the client process.")

(defparameter *keep-order* nil
  "Whether order of items in input corpus should be kept.")


;; Initialising the client process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-asdf-systems (asdf-systems)
  "Loads the required asdf systems"
  (ql:quickload asdf-systems)
  (format nil "Process initialised with asdf-systems: ~a." asdf-systems))

(defun initialise-process (&key lock-file
                                output-file
                                process-fn
                                process-fn-kwargs
                                write-fn
                                keep-order)
  "Initialises the cl-pcp client process by setting the relevant global variables."
  ;; Set global variables
  (setf ;*lock-file* lock-file
        *output-file* output-file
        *process-fn* process-fn
        *process-fn-kwargs* process-fn-kwargs
        *write-fn* write-fn
        *keep-order* keep-order)
  ;; Delete the lock file
  (loop with successful = nil until successful
        do (setf successful (delete-file lock-file)))
  (format nil "Process initialised with lock-file: ~s, temporary output-file: ~s, process-fn: ~a, process-fn-kwargs: ~a, write-fn: ~a and keep-order: ~a."
          lock-file output-file process-fn process-fn-kwargs write-fn keep-order))

(defun run (item-nr input lock-file)
  "Runs the client process for input."
  (let* ((processed-input
          (if (null *process-fn-kwargs*)
            (funcall *process-fn* input)
            (apply *process-fn* input *process-fn-kwargs*))))
    ;; Now write the output
    (with-open-file (stream *output-file* :direction :output :if-does-not-exist :create :if-exists :append)
      (if *keep-order*
        (format stream "~a.~a~%" item-nr (funcall *write-fn* processed-input))
        (format stream "~a~%" (funcall *write-fn* processed-input)))
      (force-output stream)
      (finish-output stream)))
  ;; Finally, unlock the process by deleting the lock file.
  (let ((lock-file-path (parse-namestring lock-file)))
    (loop with successful = nil until successful
          do (setf successful (delete-file lock-file-path))))
  (format nil "Item ~a done." item-nr))

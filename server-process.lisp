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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                     ;;
;; This file implements the server process side of the :cl-pcp package ;;
;;                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default directories and files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro compile-truename ()
  `',*compile-file-truename*)

(defvar *this-directory*
  (make-pathname :directory (pathname-directory (compile-truename))))

(defun cl-pcp-temp-directory ()
  (let ((temp-directory
         (make-pathname :directory (append (pathname-directory *this-directory*) (list ".tmp")))))
    (ensure-directories-exist temp-directory)
    temp-directory))

;; External Lisps ;;
;;;;;;;;;;;;;;;;;;;;

(defparameter *external-lisps*
  '((:sbcl . ((:command . "sbcl")
              (:arguments . ("--dynamic-space-size 2000" "--noinform" "--disable-debugger"))))
    (:ccl . ((:command . "ccl")
             (:arguments . ("--batch" "--quiet"))))
    (:lispworks . ((:command . "lispworks8")
                   (:arguments . ()))))
  "Configurations for starting up external common lisp processes.")

(defun external-lisp (lisp)
  "Returns all info for starting up an external lisp."
  (cdr (assoc lisp *external-lisps*)))

(defvar *external-lisp*
  #+sbcl :sbcl #+ccl :ccl #+lispworks :lispworks)


;; Client process ;;
;;;;;;;;;;;;;;;;;;;;

(defclass client-process ()
  ((id :accessor id :initarg :id :initform nil)
   (lisp :accessor lisp :initarg :lisp :initform *external-lisp*)
   (process :accessor process)
   (reader-thread :accessor reader-thread)
   (lock-file :accessor lock-file :initarg :lock-file :initform nil)
   (output-file :accessor output-file :initarg :output-file :initform nil)
   (process-fn-name :accessor process-fn :initarg :process-fn :initform 'identity)
   (process-fn-kwargs :accessor process-fn-kwargs :initarg :process-fn-kwargs :initform nil)
   (write-fn-name :accessor write-fn :initarg :write-fn :initform 'write-line-to-file)
   (keep-order :accessor keep-order :initarg :keep-order :initform nil)
   (temp-directory :accessor temp-directory :initarg :temp-directory :initform nil))
  (:documentation "Class representing client process."))

(defmethod print-object ((client-process client-process) stream)
  "Prints a client process."
  (format stream "<client-process ~(~a~), ~(~a~), ~(~a~)>"
          (id client-process) (lisp client-process)
          (uiop/launch-program:process-alive-p (process client-process))))

(defmethod initialize-instance :after ((client-process client-process) &key)
  "Initializing a client-process object."
  ;; ID
  (cond ((and (id client-process) (symbolp (id client-process))))
        ((null (id client-process))
         (setf (id client-process) (intern (symbol-name (gensym "CLIENT-PROCESS-")))))
        (t (error "The id of a client-process should be a symbol.")))
  ;; Process
  (setf (process client-process)
        (uiop/launch-program:launch-program
         (cons (cdr (assoc :command (external-lisp (lisp client-process))))
               (cdr (assoc :arguments (external-lisp (lisp client-process)))))
         :input :stream
         :output :stream
         :error-output :output))
  ;; Reader thread
  (setf (reader-thread client-process)
        (make-thread (lambda ()
                       (loop for line = (read-line (uiop/launch-program:process-info-output (process client-process)) nil)
                             while line
                             do (format t "~a: ~A~%" (id client-process) line)))
                     :name (format nil "Reader thread for ~a" (id client-process))))
  ;; Lock-file
  (unless (lock-file client-process)
    (setf (lock-file client-process)
          (merge-pathnames 
           (make-pathname :name (format nil "~(~a~)-~a-~a"
                                        (id client-process)
                                        (get-universal-time)
                                        (random 100))
                          :type "lock")
           (temp-directory client-process))))
  (when (probe-file (lock-file client-process))
    (safe-delete-file (lock-file client-process)))
  ;; Output-file
  (unless (output-file client-process)
    (setf (output-file client-process)
          (merge-pathnames
           (make-pathname :name (format nil "~(~a~)" (id client-process))
                          :type "out")
           (temp-directory client-process))))
  (when (probe-file (output-file client-process))
    (safe-delete-file (output-file client-process))))

(defmethod terminate ((client-process client-process))
  "Terminates a client-process cleanly."
  (when (probe-file (lock-file client-process))
    (safe-delete-file (lock-file client-process)))
  (uiop/launch-program:terminate-process (process client-process))
  (join-thread (reader-thread client-process)))

(defun locked (client-process)
  "Is client process locked?"
  (probe-file (lock-file client-process)))

(defun idle (client-process)
  "Is client process idle?"
  (not (probe-file (lock-file client-process))))

(defun lock (client-process)
  "Lock client process."
  (let ((lock-file
         (merge-pathnames 
          (make-pathname :name (format nil "~(~a~)-~a-~a"
                                       (id client-process)
                                       (get-universal-time)
                                       (random 100))
                         :type "lock")
          (temp-directory client-process))))
    (setf (lock-file client-process) lock-file)
    (with-open-file (stream lock-file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (write (get-internal-real-time) :stream stream)
      (force-output stream)
      (finish-output stream))))

(defun unlock (client-process)
  "Unlock client process."
  (when (probe-file (lock-file client-process))
    (safe-delete-file (lock-file client-process))))

(defun format-kwargs-list (kwargs)
  "Helper function for formatting the keyword
   arguments passed to the client function.
   Keyword arguments should be formatted as
   (:key value) where value can be another keyword,
   a symbol, a string, or a list of any of the
   these elements."
  (loop for elem in kwargs
        collect
          (cond ((keywordp elem)
                 (format nil ":~a" elem))
                ((symbolp elem)
                 (format nil "~a::~a" (package-name (symbol-package elem)) elem))
                ((stringp elem)
                 (format nil "\"~a\"" elem))
                ((listp elem)
                 (format-kwargs-list elem))
                (t elem))))

(defun spawn-client-process (&key (id (intern (symbol-name (gensym "CLIENT-PROCESS-"))))
                                  (external-lisp *external-lisp*)
                                  asdf-systems
                                  (process-fn 'identity)
                                  process-fn-kwargs
                                  (write-fn 'write-line-to-file)
                                  keep-order
                                  temp-directory)
  "Starts up a client-process and intialises it."
  ;; Start the client process.
  (let ((client-process (make-instance 'client-process
                                       :id id
                                       :lisp external-lisp
                                       :process-fn process-fn
                                       :process-fn-kwargs process-fn-kwargs
                                       :write-fn write-fn
                                       :keep-order keep-order
                                       :temp-directory temp-directory)))
    ;; check it
    (unless (uiop/launch-program:process-alive-p (process client-process))
      (error "Could not start client process '~a ~{~a~^ ~} :input :stream :output :stream'"
             (cdr (assoc :command (external-lisp external-lisp)))
             (cdr (assoc :arguments (external-lisp external-lisp))))
      (terminate client-process))
    ;; initialise it
    (lock client-process)
    (write-to-client-process client-process "(ql:quickload :cl-pcp)")
    (write-to-client-process client-process "(in-package :cl-pcp)")
    ;; If ever, the web-interface system is loaded, do not start it automatically
    (write-to-client-process client-process "(setf cl-user::*automatically-start-web-interface* nil)")
    (write-to-client-process client-process (format nil "(load-asdf-systems '(~{~s~^ ~}))" asdf-systems))
    (write-to-client-process client-process
                             (format nil "(initialise-process :lock-file ~s :output-file ~s :process-fn '~a :process-fn-kwargs '~a :write-fn '~a :keep-order ~a)"
                                      (lock-file client-process) (output-file client-process)
                                      (format nil "~a::~a" (package-name (symbol-package (process-fn client-process))) (process-fn client-process))
                                      (format-kwargs-list process-fn-kwargs)
                                      (format nil "~a::~a" (package-name (symbol-package (write-fn client-process))) (write-fn client-process))
                                      (keep-order client-process)))
    client-process))


;; High-level interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-corpus-in-parallel (input-file output-file &key
                                              (external-lisp *external-lisp*)
                                              (asdf-systems nil)
                                              (read-fn 'read-line-from-stream)
                                              (process-fn 'identity)
                                              (process-fn-kwargs nil)
                                              (write-fn 'identity)
                                              (nr-of-processes 4)
                                              (keep-order nil)
                                              temp-directory)
  ;; Printing some information
  (format t "~%~%****************** Started Corpus Processing ******************")
  (format t "~%~%Inputfile: ~a" input-file)
  (format t "~%Outputfile: ~a" output-file)
  (format t "~%Temp directory: ~a" (or temp-directory (cl-pcp-temp-directory)))
  (format t "~%ASDF systems: ~a" asdf-systems)
  (format t "~%Read function: ~a" read-fn)
  (format t "~%Applying function: ~a" process-fn)
  (format t "~%Passing function arguments: ~a" process-fn-kwargs)
  (format t "~%Write function: ~a" write-fn)
  (format t "~%Processes: ~a" nr-of-processes)
  (format t "~%Keep order: ~a" keep-order)
  ;; First make sure all fasl files exist, so that the client processes do not get confused with each others compiled files
  (ql:quickload asdf-systems)
  ;; First start up all the client-processes
  (let ((processes (loop for i from 1 upto nr-of-processes
                         collect (spawn-client-process :id (intern (format nil "CLIENT-PROCESS-~a" i))
                                                       :external-lisp external-lisp
                                                       :asdf-systems asdf-systems
                                                       :process-fn process-fn
                                                       :process-fn-kwargs process-fn-kwargs
                                                       :write-fn write-fn
                                                       :keep-order keep-order
                                                       :temp-directory (or temp-directory (cl-pcp-temp-directory))))))
    (unwind-protect
        (progn
          ;; Now, open the corpus
          (with-open-file (corpus input-file :direction :input)
            ;; Loop through the corpus
            (loop with cycle-pos = nil
                  for input-item = (funcall read-fn corpus)
                  for item-nr from 1
                  ;; until you're through
                  while input-item
                  ;; Select an idle client-process, potentially waiting for one...
                  for idle-process = (wait-for-idle-client-process processes cycle-pos) 
                  do
                  ;; Process the current corpus item on this process and lock it
                  (setf cycle-pos (1+ (position idle-process processes)))
                  (lock idle-process)
                  (write-to-client-process
                   idle-process
                   (format nil "(run ~a ~a ~s)"
                           item-nr
                           (if (stringp input-item)
                             (format nil "~s" input-item)
                             input-item)
                           (lock-file idle-process)))))
          
          ;; Make sure to wait until all processes have finished
          (wait-until-all-process-idle processes)
          
          ;; Then, aggregate all the output data and optionally sort everything
          (format t "~%Aggregating output of client processes...~%" output-file)
          (if keep-order
            (progn (format t "~%Sorting output...~%")
              (let* ((output-data (loop for temp-file in (mapcar #'output-file processes)
                                        when (probe-file temp-file)
                                        append
                                            (with-open-file (temp-stream temp-file :direction :input)
                                              (loop for output-line = (read-line temp-stream nil)
                                                    while output-line
                                                    collect (let ((output-parts (multiple-value-list (split-sequence #\. output-line :count 1))))
                                                              (cons (parse-integer (first (first output-parts)))
                                                                    (subseq output-line (second output-parts))))))))
                     (output-data-array (let ((array (make-array (length output-data))))
                                          (loop for el in output-data
                                                do (setf (aref array (- (car el) 1))
                                                         (cdr el))
                                                finally (return array)))))
                (with-open-file (output-stream output-file :direction :output :if-does-not-exist :create :if-exists :supersede)
                  (loop for el across output-data-array
                        do (format output-stream "~a~%" el)))))
            (uiop/stream:concatenate-files (loop for file in (mapcar #'output-file processes)
                                                 when (probe-file file)
                                                   collect file) output-file))
          
          (loop for file in (mapcar #'output-file processes)
                when (probe-file file)
                do (delete-file file))
          (format t "~%Output written to: ~a~%" output-file)
          
          ;; Finally kill the processes
          (format t "~%Closing client processes...~%~%")
          (mapcar #'terminate processes)
          (format t "~%Finished!~%"))
      (mapcar #'terminate processes))))

(defun wait-for-idle-client-process (client-processes cycle-pos)
  "Returns an idle process, potentially waiting until one is available."
  (when cycle-pos
    (setf client-processes
          (append (subseq client-processes cycle-pos)
                  (subseq client-processes 0 cycle-pos))))  
  (loop with idle-process = nil
        until idle-process
        do (setf idle-process
                 (loop for process in client-processes
                       if (idle process)
                       return process
                       else do (sleep 0.1)))
        finally (return idle-process)))

(defun wait-until-all-process-idle (client-processes)
  "Returns t once all processes are idle."
  (loop until (loop for process in client-processes
                    always (idle process))
        do (sleep 0.1)
        finally (return t)))
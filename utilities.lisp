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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                           ;;
;; Utility functions for the cl-pcp package. ;;
;;                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-from-stream (stream)
  "Reads s-expression from stream or returns nil if end-of-stream."
  (read stream nil))

(defun read-line-from-stream (inputstream)
  "Reads string from stream or returns nil if end-of-stream."
  (read-line inputstream nil))

(defun write-to-client-process (client-process string)
  "Writes a command to a client-process."
  (write-line-to-stream (uiop/launch-program:process-info-input (process client-process)) string))

(defun write-line-to-stream (outputstream result)
  "Writes a line to a stream and flushes."
  (format outputstream "~a~%" result)
  (force-output outputstream))

;; For demo purposes.
(defun use-cpu (load)
  "Uses CPU for about load seconds (on i7)."
  (loop for i from 1 upto (* load 1000000000)
        do (* i load)
        finally (return load)))

;; For demo purposes.
(defun use-cpu-with-kwargs (load &key symbol-key string-key number-key list-key)
  (print symbol-key)
  (print string-key)
  (print number-key)
  (print list-key)
  (loop for i from 1 upto (* load 1000000000)
        do (* i load)
        finally (return load)))

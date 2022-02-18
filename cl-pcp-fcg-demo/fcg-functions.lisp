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

(in-package :fcg)

(export '(comprehend-using-demo-grammar-with-timeout encode-fcg-meaning-as-json))

(load-demo-grammar)

;;Example of a process function
(defun comprehend-using-demo-grammar-with-timeout (utterance)
  "Comprehends an utterance using FCG's demo grammar"

  ;;Call comprehend 500 times with a timeout of 5 seconds
  (handler-case (trivial-timeout:with-timeout (5)
                  (loop repeat 500
                        collect (fcg:comprehend utterance) into results
                        finally (return (last-elt results))))
    (trivial-timeout:timeout-error (error)
      'timeout)))


;;Example of a write function
(defun encode-fcg-meaning-as-json (meaning)
  "Encode meaning as json"
  (if (eq meaning 'timeout)
    (cl-json:encode-json-alist-to-string
    '((:meaning . time-out)))
    (cl-json:encode-json-alist-to-string
     `((:meaning . ,(utils:list-of-predicates->string-of-predicates meaning))))))
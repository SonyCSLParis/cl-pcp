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

(defpackage :cl-pcp
  (:use :cl)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread)
  (:export :process-corpus-in-parallel
           :read-from-stream
           :read-line-from-stream)
  (:documentation "A package for parallel corpus processing."))
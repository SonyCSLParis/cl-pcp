# cl-pcp
Common Lisp Package for Parallel Corpus Processing

Parallel Corpus Processing in Common Lisp. This documentation is mainly written for users of our open-source language technologies (the Babel software platform: https://emergent-languages.org/, which includes Fluid Construction Grammar), but the code can be used for any corpus processing task in Common Lisp.

## Installation and Use

In order to use the :cl-pcp system, the folder cl-pcp should be moved to a place where it can be found by asdf. For use in combination with Babel, we recommend to move it to Babel’s systems folder.

The :cl-pcp system has been tested using Clozure Common Lisp (CCL) version 1.12 and Steel Bank Common Lisp (SBCL) version 2.0.11. The code is also expected to be compatible with LispWorks 7.1.x but has not been tested on this platform.
A demonstration of the possibilities is included in the ﬁle demo.lisp.

## Requirement Analysis

The package should be able to process a corpus in parallel. This involves reading each individual item from an input corpus ﬁle, processing it, and writing the output to an output corpus ﬁle. As all items of a corpus are independent from each other, processing can be parallelised on multi-core machines.
:cl-pcp should be able to overcome the two main bottlenecks in Babel’s previous :corpus-processing package:

* :cl-pcp should make use of subprocesses instead of threads, so that the single-threaded memory management of common lisp implementations does not form a bot-tleneck anymore.
* :cl-pcp should avoid a batch-based approach in which threads sometimes need to wait for each other. This is of particular importance when processing large text corpora using Fluid Construction Grammar, where it has been observed that utterances requiring long processing times are often located near each other in the corpus (due to eﬀects of same genre, origin, length, etc.).

## General Architecture

On a high-level, the :cl-pcp system operates as follows:

* First of all, the user opens a common lisp session, (quick)loads the :cl-pcp system as well as any other systems that the user’s code relies on and enters the :cl-pcp package. Then, they call the high-level interface function process-corpus-in-parallel as follows:

````
(process-corpus-in-parallel input-file
                            output-file
                            :external-lisp *external-lisp* 
                            :asdf-systems ’()
                            :read-fn ’read-from-stream 
                            :process-fn ’identity 
                            :write-fn ’identity 
                            :nr-of-processes 8 
                            :keep-order nil)
````

* The cl-pcp system then makes as many client-processes as speciﬁed using the nr-of-processes key. Each client process is an independent lisp session using the lisp implementation speciﬁed by the :external-lisp key.
* The client process is initialised by ﬁrst loading the :cl-pcp system, entering the :cl-pcp package, and loading the systems speciﬁed by the :asdf-systems key.
* The main process then opens the input-file and applies the function speciﬁed by the :read-fn key to the resulting stream, until the end of the stream has been reached.
* After each read operation, the result is passed to an idle client process, where the function speciﬁed by the :process-fn key is applied to it.
* Then, the result of the processing operation is optionally transformed by the function speciﬁed by the :write-fn key and written by the client process to an automatically created temporary output ﬁle.
* Finally, after all items from the corpus have been processed, the output of all the client-processes is aggregated and written to output-file. If :keep-order is true, the output is guaranteed to appear in the same order as the input. This requires reading the entire corpus into memory, so it should be avoided with ﬁles that exceed the memory limit of the machine it is processed on.

## High-Level Interface

In a call to process-corpus-in-parallel as depicted above, only input-ﬁle and output-ﬁle are required. However, optionally, other keyword arguments can be passed. The arguments, options and defaults for the process-corpus-in-parallel function are the following:
* input-file. Pathname pointing to the input corpus.
* output-file. Pathname pointing to a ﬁle where the output corpus will be written. The ﬁle is created if it does not yet exist, otherwise this ﬁle is overwritten.
* :external-lisp. The lisp implementation to be used by the client processes. Possible options are :ccl, :sbcl or :lispworks. The default value is *external-lisp*, which is bound to the implementation used by the user for starting up the main process.
* :asdf-systems. A list of all asdf systems to be loaded by the client processes. Note that this should load all code that is needed to apply :process-fn and :write-fn. The default is NIL.
* :read-fn. The function that is used to read from the stream connected to the input corpus. Takes as its only argument a stream. Typically, ’read-line-from-stream is used if each line of the input corpus should be read in as a string and ’read-from-stream is used if each element in the input corpus should be read in as an s-expression. The default is ’read-line-from-stream .
* :process-fn. A symbol referring to the function that will be applied by the client processes to the result of the reading operations. Default is ’identity. This will essentially copy the corpus. The process function gets as its only argument the input item from the corpus and should return the processed item.
* :write-fn. A symbol referring to a function that is called on the result of the process-ing operation, before writing the output to the output corpus. Default is ’identity, so nothing is done. Useful if output should be encoded to a JSON string. The write function gets as its only input argument the output from the process function and should return the processed item as it should be written to the output corpus.
* :nr-of-processes. The number of subprocesses that will be created. Typically close to the number of cores of the machine that is used. Default is 4.
* :keep-order. If true, then the order of the items in the input corpus is kept in the output corpus. This requires loading the entire output corpus into memory. If the items in a corpus are truly independent, it is preferable to keep an id in the output format instead of relying on the order.

## Process Management

The communication between the main process and the sub-processes concerning whether they are idle or busy happens through so-called lock-ﬁles. When the main process sends commands to a client process, it locks it by creating a ﬁle with the name of the client process and the ﬁle extension .lock. When the client process is ready, it deletes the lock ﬁle. This way, the main process knows that it is open to receive new input.

## Hints for a Successful Usage

* Make sure that all code that needs to be accessible by the client processes is loaded when evaluating the asdf systems that are speciﬁed.
* Make sure that the values passed to :process-fn and :write-fn are symbols referring to a function and not the function itself (e.g. ’identity instead of #’identity).
* Make sure that the external lisps that you use are accessible under the name lispworks, ccl or sbcl in the terminal of your operating system.
* Monitor the REPL / output browser of the parent process. All output by the client processes, including errors, is redirected here.
* If things go wrong, kill the thread on which the high-level interface function runs. This also shuts down all client processes.
* Always load the asdf systems for the client processes ﬁrst once in the main process (also when making any changes). This makes sure that all the compiled ﬁles are in place and avoids getting ”fasl ﬁle not found” errors on one or more client processes. Spawning client processes is also much faster then. If the client processes use a diﬀerent lisp than the main process, make sure to compile the asdf systems once using the lisp of the sub-processes.

## Demonstration with an FCG Demo Grammar

For users who have Babel installed, an additional system is provided for demonstration purposes: :cl-pcp-fcg-demo. This system (quick)loads :fcg, :cl-json and :trivial-timeout. The only ﬁle that it contains, fcg-functions.lisp, holds a process function and a write function that are used by the high-level interface function process-corpus-in-parallel in the demo.lisp ﬁle of the main cl-pcp folder. It also loads FCG’s built-in demo grammar.
The process function comprehend-using-demo-grammar-with-timeout works on single utterances that can be comprehended by FCG’s demo grammar. It makes 500 calls to comprehend with the utterance as argument and returns the result of the ﬁnal call. We make use of a time-out of 5 seconds. This means that when the 500 calls cannot be handled within 5 seconds, the symbol ’timeout will be returned by the function, instead of a list of meaning predicates.
The write function encode-fcg-meaning-as-json will in turn encode the resulting meaning in the form of a JSON object with a key :meaning and a string of predicates as its value (e.g. “unique(?y-19176), mouse(?y-19176), deep-aﬀection(?x-54368, ?y-19176), linguist(?x-54368), unique(?x-54368)”). We make use of Babel’s function utils:list-of-predicates-¿string-of-predicates to do the list to string conversion. If a time-out occurred in the process function, the value will simply be the string“time-out”.
We export both functions here so they are easily accessible by the :cl-pcp package. However, it is not strictly needed to do this, and one can also pass non-external symbols by making use of common lisp’s double colon syntax.

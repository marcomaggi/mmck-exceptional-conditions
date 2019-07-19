;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: coroutines implementation
;;;Date: Jul 19, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines coroutines.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(declare (unit mmck.exceptional-conditions.coroutines)
	 (uses mmck.exceptional-conditions.helpers)
	 (uses mmck.exceptional-conditions.condition-objects)
	 (uses mmck.exceptional-conditions.handlers)
	 (uses mmck.exceptional-conditions.unwind-protection)
	 (emit-import-library mmck.exceptional-conditions.coroutines))

(module (mmck.exceptional-conditions.coroutines)
  (coroutine
      yield finish-coroutines
      current-coroutine-uid coroutine-uid?
      suspend-coroutine resume-coroutine suspended-coroutine?
      reset-coroutines! dump-coroutines
      ;;This is for internal use.
      dequeue!)
  (import (scheme)
    (prefix (chicken plist)
	    chicken::)
    (mmck exceptional-conditions helpers)
    (mmck exceptional-conditions condition-objects)
    (mmck exceptional-conditions handlers)
    (mmck exceptional-conditions unwind-protection))
  (import (only (chicken module) reexport))
  (import-for-syntax (scheme)
		     (only (chicken syntax)
			   er-macro-transformer)
		     (only (matchable)
			   match)
		     (mmck exceptional-conditions helpers))


;;;; helpers

(define (get-coroutine-state uid)
  (chicken::get uid 'mmck-coroutine-uid #f))

(define (set-coroutine-state! uid val)
  (chicken::put! uid 'mmck-coroutine-uid val))


;;;; coroutine continuations queue

;;The value of this parameter is #f or a pair representing a queue of escape functions.
;;
;;The car  of the queue-pair  is the first  pair of the  list of escape  functions.  The cdr  of the
;;queue-pair is the last pair of the list of escape functions.
;;
;;NOTE At  present Vicare  has no  native threads,  so this QUEUE  variable could  well be  a simple
;;binding.  To  show off: we make  it a parameter because  that would be needed  in a multithreading
;;process.
;;
(define queue
  (make-parameter #f))

(define (empty-queue?)
  (not (queue)))

(define (enqueue! escape-func)
  (cond ((queue)
	 => (lambda (Q)
	      (let ((old-last-pair (cdr Q))
		    (new-last-pair (list escape-func)))
		(set-cdr! old-last-pair new-last-pair)
		(set-cdr! Q             new-last-pair))))
	(else
	 (let ((Q (list escape-func)))
	   (queue (cons Q Q))))))

(define (dequeue!)
  (cond ((queue)
	 => (lambda (Q)
	      (let ((head (car Q)))
		(begin0
		    (car head)
		  (let ((head (cdr head)))
		    (if (null? head)
			(reset-coroutines!)
		      (set-car! Q head)))))))
	(else
	 (error 'dequeue! "no more coroutines"))))

(define (reset-coroutines!)
  (queue #f))

(define (dump-coroutines)
  (debug-print 'enqueued-coroutines
	       (cond ((queue)
		      => car)
		     (else #f))))


;;;; unique identifier

(define %current-coroutine-uid
  (make-parameter (gensym "main-coroutine-uid")))

(define (current-coroutine-uid)
  ;;Return a gensym acting as unique identifier for the current coroutine.
  ;;
  ;;We do not want to expose the parameter in the public API: it must be immutable in the user code.
  ;;
  (%current-coroutine-uid))

(define (coroutine-uid? obj)
  (and (symbol? obj)
       (coroutine-state? (get-coroutine-state obj))))


;;;; coroutine state

(define-record <coroutine-state>
  reinstate-procedure
		;False or a procedure that reinstates the coroutine continuation.  It
		;is used when suspending a coroutine.
  )

(define coroutine-state? <coroutine-state>?)

(define-syntax-rule (coroutine-state-reinstate-procedure ?obj)
  (<coroutine-state>-reinstate-procedure ?obj))

(define-syntax-rule (coroutine-state-reinstate-procedure-set! ?obj ?val)
  (<coroutine-state>-reinstate-procedure-set! ?obj ?val))

(define (make-coroutine-state)
  (make-<coroutine-state> #f))

(define (coroutine-state uid)
  ;;Given a coroutine UID: return the associated state procedure.
  ;;
  (assert (symbol? uid))
  (or (%coroutine-state uid)
      (assertion-violation 'coroutine-state
	"expected symbol with coroutine state in value slot as coroutine UID argument"
	uid)))

(define (%coroutine-state uid)
  ;;Given a coroutine UID: return the associated state procedure.
  ;;
  (let ((val (get-coroutine-state uid)))
    (and (coroutine-state? val)
	 val)))


;;;; basic operations

(define (%enqueue-coroutine thunk)
  (call/cc
      (lambda (reenter)
	(enqueue! reenter)
	(thunk)
	;;Dequeue the next escape procedure and apply it to void, so that it returns one value.
	((dequeue!) (void)))))

(define (coroutine thunk)
  ;;Create a new coroutine having THUNK as function and enter it.  Return unspecified values.
  ;;
  (define uid (gensym "coroutine-uid"))
  (set-coroutine-state! uid (make-coroutine-state))
  (parameterize
      ((run-unwind-protection-cleanup-upon-exit?	#f)
       (%current-coroutine-uid				uid))
    (%enqueue-coroutine thunk)))

(define (yield)
  ;;Register the current continuation as coroutine, then run the next coroutine.  Return unspecified
  ;;values.
  ;;
  (%enqueue-coroutine void))

(case-define finish-coroutines
  (()
   ;;Loop running the next coroutine until there are no more.  Return unspecified values.
   ;;
   (unless (empty-queue?)
     (yield)
     (finish-coroutines)))
  ((exit-loop?)
   ;;Loop running the next coroutine until there are  no more or the thunk EXIT-LOOP?  returns true.
   ;;Return unspecified values.
   ;;
   (unless (or (empty-queue?)
	       (exit-loop?))
     (yield)
     (finish-coroutines exit-loop?))))


;;;; suspending and resuming

(define (suspended-coroutine? uid)
  ;;Return true if UID is the unique identifier of a suspended coroutine.
  ;;
  (assert (coroutine-uid? uid))
  (let ((state (%coroutine-state uid)))
    (and (coroutine-state-reinstate-procedure state)
	 #t)))

(define (suspend-coroutine)
  ;;Suspend the  current coroutine.  Yield  control to  the next coroutine,  but do not  enqueue the
  ;;current continuation to be reinstated later.
  ;;
  (let ((state (%coroutine-state (current-coroutine-uid))))
    (cond ((coroutine-state-reinstate-procedure state)
	   => (lambda (reinstate)
		(assertion-violation 'suspend-coroutine
		  "attempt to suspend an already suspended coroutine"
		  (current-coroutine-uid))))
	  (else
	   (call/cc
	       (lambda (escape)
		 (coroutine-state-reinstate-procedure-set! state escape)
		 ((dequeue!) (void))))))))

(define (resume-coroutine uid)
  ;;Resume a previously suspended coroutine.
  ;;
  (assert (coroutine-uid? uid))
  (let ((state (%coroutine-state uid)))
    (cond ((coroutine-state-reinstate-procedure state)
	   => (lambda (reinstate)
		(coroutine-state-reinstate-procedure-set! state #f)
		(%enqueue-coroutine (lambda ()
				      (reinstate (void))))))
	  (else
	   (assertion-violation 'resume-coroutine
	     "attempt to resume a non-suspended coroutine" uid)))))


;;;; done

  #| end of module |# )

;;; end of file

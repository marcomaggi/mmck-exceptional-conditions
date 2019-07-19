;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: stacks of compensations for asynchronous resources
;;;Date: Jul 19, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the compensations mechanism.
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

(declare (unit mmck.exceptional-conditions.compensations)
	 (uses mmck.exceptional-conditions.helpers)
	 (uses mmck.exceptional-conditions.handlers)
	 (emit-import-library mmck.exceptional-conditions.compensations))

(module (mmck.exceptional-conditions.compensations)
    (run-compensations
     push-compensation-thunk
     ;; private exports
     compensations
     run-compensations-store)
  (import (scheme)
	  (mmck exceptional-conditions handlers)
	  (mmck exceptional-conditions helpers))
  (import-for-syntax (scheme)
		     (only (chicken syntax)
			   er-macro-transformer)
		     (only (matchable)
			   match)
		     (mmck exceptional-conditions helpers))


;;;; compensations

(define %invalid-compensations-store
  (case-lambda
    (()
     (assertion-violation 'compensations
       "attempt to retrieve compensations outside of compensations environment"))
    ((false/compensation-thunk)
     (assertion-violation 'compensations
       "attempt to register compensation outside of compensations environment"))))

;;References a function which:
;;
;;* When called with no arguments returns the list of current compensation thunks.
;;
;;* When called with a #f argument: resets to null the list of current compensation thunks.
;;
;;* When called with a thunk argument: pushes it on the list of compensation thunks.
;;
(define compensations
  (make-parameter %invalid-compensations-store))

(define (run-compensations)
  ;;Run the compensations in the current compensations store.
  ;;
  (run-compensations-store (compensations)))

(define (run-compensations-store store)
  ;;Run the  compensations in the STORE,  which must be a  store function.  After running  reset the
  ;;compensations stack to empty.
  ;;
  (with-exception-handler
      (lambda (dummy)
	(assertion-violation 'run-compensations-store
	  "expected null or proper list in compensations parameter"
	  (store)))
    (lambda ()
      (for-each-in-order
	  (lambda (closure)
	    ;;We want to block and discard exceptions raised by CLOSURE.
	    (call/cc
		(lambda (escape)
		  (with-exception-handler escape closure))))
	(store))))
  (store #f)
  (values))

(define (push-compensation-thunk compensation-thunk)
  ;;Push a compensation thunk on the current store.
  ;;
  (assert (procedure? compensation-thunk))
  ((compensations) compensation-thunk)
  (values))


;;;; done

#| end of module |# )

;;; end of file

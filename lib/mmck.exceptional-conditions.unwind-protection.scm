;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: unwind protection mechanism
;;;Date: Jul 19, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines some basic facitilies for the unwind protection mechanism.
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

(declare (unit mmck.exceptional-conditions.unwind-protection)
	 (uses mmck.exceptional-conditions.helpers)
	 (uses mmck.exceptional-conditions.condition-objects)
	 (uses mmck.exceptional-conditions.handlers)
	 (emit-import-library mmck.exceptional-conditions.unwind-protection))

(module (mmck.exceptional-conditions.unwind-protection)
  (unwinding-call/cc
   ;;; private exports
   run-unwind-protection-cleanup-upon-exit?)
  (import (scheme)
	  (mmck exceptional-conditions helpers)
	  (mmck exceptional-conditions condition-objects)
	  (mmck exceptional-conditions handlers))


;;;; unwind protection infrastructure

(define run-unwind-protection-cleanup-upon-exit?
  ;;This is used in the interaction between the unwind-protection mechanism and the GUARD syntax.
  ;;
  (make-parameter #f))

(define* (unwinding-call/cc receiver)
  ;;Performing a raw escape from an exception handler skips calling the unwind handlers installed in
  ;;the  body; this  problem can  be  solved by  using  UNWINDING-CALL/CC rather  than the  standard
  ;;CALL/CC.
  ;;
  ;;Similar  to CALL/CC,  but calling  the  escape procedure  causes  the invocation  of the  unwind
  ;;handlers installed in the dynamic environment up until the saved continuation is restored.
  ;;
  ;;There are limitations:
  ;;
  ;;* The escape procedure produced by this primitive  *must* be called only from the dynamic extent
  ;;of the call to  RECEIVER.  For example: generating an unwinding escape  procedure in a coroutine
  ;;and calling it from another coroutine leads to raising an exception of type "&non-reinstatable".
  ;;
  ;;* The escape procedure produced by this primitive *must* be called only once; an attempt to call
  ;;it a second time leads to raising an exception of type "&non-reinstatable".
  ;;
  ;;NOTE After some development  iterations, the implementation of this primitive  has taken a shape
  ;;quite   similar   to    the   function   CALL/CC-ESCAPING   proposed   by    Will   Clinger   in
  ;;<http://www.ccs.neu.edu/home/will/UWESC/uwesc.sch>.
  ;;
  (let ((inside-dynamic-extent-of-receiver-call? #f)
	(escape-procedure-already-called-once?   #f))
    (dynamic-wind
	(lambda ()
	  (set! inside-dynamic-extent-of-receiver-call? #t))
	(lambda ()
	  (begin0
	      (call/cc
		  (lambda (escape)
		    (receiver (lambda retvals
				(if inside-dynamic-extent-of-receiver-call?
				    (if escape-procedure-already-called-once?
					(non-reinstatable-violation __who__
					  "unwinding escape procedure called for the second time")
				      (begin
					;;Yes,  we  must really  set  the  parameter to  the  symbol
					;;"escape"; this symbol  is used as argument  for the unwind
					;;handlers.
					(run-unwind-protection-cleanup-upon-exit? 'escape)
					(set! escape-procedure-already-called-once? #t)
					(apply escape retvals)))
				  (non-reinstatable-violation __who__
				    "unwinding escape procedure called outside the dynamic extent of its receiver function"))))))
	    (run-unwind-protection-cleanup-upon-exit? #f)))
	(lambda ()
	  (set! inside-dynamic-extent-of-receiver-call? #f)))))


;;;; done

  #| end of module |# )

;;; end of file

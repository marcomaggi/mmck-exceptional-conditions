;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: exceptios-related macros
;;;Date: Jul 17, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines some macros related to exception handling and the dynamic environment.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is  free software: you can redistribute  it and/or modify it under the  terms of the
;;;GNU Lesser General Public License as published  by the Free Software Foundation, either version 3
;;;of the License, or (at your option) any later version.
;;;
;;;This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;;;even the implied  warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
;;;Lesser General Public License for more details.
;;;
;;;You should have received a copy of the GNU Lesser General Public License along with this program.
;;;If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(declare (unit mmck.exceptional-conditions.macros)
	 (uses mmck.exceptional-conditions.helpers)
	 (uses mmck.exceptional-conditions.condition-objects)
	 (uses mmck.exceptional-conditions.handlers)
	 (emit-import-library mmck.exceptional-conditions.macros))

(module (mmck.exceptional-conditions.macros)
    ((syntax: return
	      mmck-return-handler)
     (syntax: returnable
	      unwinding-call/cc mmck-return-handler)
     (syntax: with-unwind-handler
	      dynamic-wind
	      non-reinstatable-violation
	      run-unwind-protection-cleanup-upon-exit?
	      call/cc
	      with-exception-handler)
     (syntax: unwind-protect)
     unwinding-call/cc
     mmck-return-handler)
  (import (scheme)
	  (mmck exceptional-conditions helpers)
	  (mmck exceptional-conditions condition-objects)
	  (mmck exceptional-conditions handlers))
  (import-for-syntax (scheme)
		     (only (chicken base)
			   define-record)
		     (only (chicken syntax)
			   er-macro-transformer)
		     (only (matchable)
			   match)
		     (mmck exceptional-conditions helpers))


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


;;;; syntax RETURNABLE

(define mmck-return-handler
  (make-parameter
      (lambda args
	(error 'return "used RETURN syntax outside a syntax that binds it"))
    (lambda (obj)
      (if (procedure? obj)
	  obj
	(assertion-violation 'mmck-return-handler
	  "expected procedure as value for parameter MMCK-RETURN-HANDLER" obj)))))

(define-syntax return
  (syntax-rules ()
    ((_ ?arg ...)
     ((mmck-return-handler) ?arg ...))
    ))

(define-syntax returnable
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define %unwinding-call/cc	(rename 'unwinding-call/cc))
      (define %lambda			(rename 'lambda))
      (define %parameterize		(rename 'parameterize))
      (define %mmck-return-handler	(rename 'mmck-return-handler))

      (match input-form.stx
	((_ ?body0 ?body* ...)
	 `(,%unwinding-call/cc
	   (,%lambda (escape)
		     (,%parameterize ((,%mmck-return-handler escape))
				     ,?body0 . ,?body*))))
	(_
	 (syntax-error 'returnable "invalid syntax in macro use"))))))


;;;; syntax WITH-UNWIND-HANDLER

(define-syntax with-unwind-handler
  (er-macro-transformer
    (lambda (input-form.stx rename compare)


;;;; syntax WITH-UNWIND-HANDLER: main function and helpers

(define (main input-form.stx)
  (receive (handler.stx thunk.stx)
      (parse-input-form input-form.stx)
    (build-output-form handler.stx thunk.stx)))

(define (synner message . args)
  (apply syntax-error 'with-undind-handler message input-form.stx args))

(define (parse-input-form input-form.stx)
  (match input-form.stx
    ((_ ?handler ?thunk)
     (values ?handler ?thunk))
    (_
     (synner "invalid syntax in input form"))))


;;;; syntax WITH-UNWIND-HANDLER: syntactic identifiers needed in the output form

(define %call/cc			(rename 'call/cc))
(define %dynamic-wind			(rename 'dynamic-wind))
(define %non-reinstatable-violation	(rename 'non-reinstatable-violation))
(define %run-unwind-protection-cleanup-upon-exit? (rename 'run-unwind-protection-cleanup-upon-exit?))
(define %with-exception-handler		(rename 'with-exception-handler))

(define %begin0		(rename 'begin0))
(define %cond		(rename 'cond))
(define %if		(rename 'if))
(define %lambda		(rename 'lambda))
(define %let		(rename 'let))
(define %set!		(rename 'set!))
(define %unless		(rename 'unless))
(define %when		(rename 'when))


;;;; syntax WITH-UNWIND-HANDLER: build output form

(define (build-output-form handler.stx thunk.stx)
  `(,%let ((terminated?	#f)
		;True if the dynamic extent of the call to THUNK is terminated.
	   (normal-exit?	#f))
		;True if the dynamic extent of the call  to ?THUNK was exited by performing a normal
		;return.
	  (,%dynamic-wind
	   (,%lambda ()
		     (,%when terminated?
			     (,%non-reinstatable-violation 'with-unwind-handler "attempt to reenter thunk with terminated dynamic extent")))
	   (,%lambda ()
		     (,%begin0
		      (,thunk.stx)
		      (,%set! normal-exit? #t)))
	   (,%lambda ()
		     (,%unless terminated? ;be safe
			       (,%cond ((,%if normal-exit?
					      'return
					      ;;This parameter is set to:
					      ;;
					      ;;* The boolean #f if no unwind handler must be run.
					      ;;
					      ;;* The symbol "exception"  if the unwind handler must
					      ;;be  run because  an  exception has  been raised  and
					      ;;catched by GUARD.
					      ;;
					      ;;* The symbol "escape" if  the unwind handler must be
					      ;;run because  an unwinding escape procedure  has been
					      ;;called.
					      ;;
					      (,%run-unwind-protection-cleanup-upon-exit?))
					=> (,%lambda (why)
						     (,%set! terminated? #t)
						     ;;We want to discard any exception raised by the cleanup thunk.
						     (,%call/cc
						      (,%lambda (escape)
								(,%with-exception-handler
								 escape
								 (,%lambda ()
									   (,handler.stx why)))))))))))))


;;;; syntax WITH-UNWIND-HANDLER: let's go

(main input-form.stx))))


;;;; syntax UNWIND-PROTECT

(define-syntax unwind-protect
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define %with-unwind-protection	(rename 'with-unwind-protection))
      (define %lambda			(rename 'lambda))
      (define %why			(rename 'why))
      (match input-form.stx
	((_ ?body ?cleanup0 ?cleanup* ...)
	 `(,%with-unwind-protection
	   (,%lambda (,%why) ,?cleanup0 . ,?cleanup*)
	   (,%lambda () ,?body)))
	(_
	 (syntax-error "invalid syntax in macro use"))))))


;;;; done

#| end of module |# )

;;; end of file

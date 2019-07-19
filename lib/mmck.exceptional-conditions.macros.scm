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


;;;; copyright notice for the XOR macro
;;;
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
;;;associated documentation  files (the "Software"),  to deal  in the Software  without restriction,
;;;including  without limitation  the  rights  to use,  copy,  modify,  merge, publish,  distribute,
;;;sublicense, and/or  sell copies of the  Software, and to permit  persons to whom the  Software is
;;;furnished to do so, subject to the following conditions:
;;;
;;;The  above copyright  notice  and this  permission  notice shall  be included  in  all copies  or
;;;substantial portions of the Software.
;;;
;;;Except as contained in this notice, the name(s)  of the above copyright holders shall not be used
;;;in advertising or otherwise  to promote the sale, use or other dealings  in this Software without
;;;prior written authorization.
;;;
;;;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
;;;NOT  LIMITED  TO  THE  WARRANTIES  OF  MERCHANTABILITY, FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
;;;NONINFRINGEMENT.  IN  NO EVENT SHALL THE  AUTHORS OR COPYRIGHT  HOLDERS BE LIABLE FOR  ANY CLAIM,
;;;DAMAGES OR OTHER  LIABILITY, WHETHER IN AN  ACTION OF CONTRACT, TORT OR  OTHERWISE, ARISING FROM,
;;;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;;; units and module header

(declare (unit mmck.exceptional-conditions.macros)
	 (uses mmck.exceptional-conditions.helpers)
	 (uses mmck.exceptional-conditions.condition-objects)
	 (uses mmck.exceptional-conditions.handlers)
	 (uses mmck.exceptional-conditions.compensations)
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
     (syntax: guard
	      call/cc
	      raise-continuable
	      run-unwind-protection-cleanup-upon-exit?
	      with-exception-handler)
     (syntax: with-blocked-exceptions
	      call/cc
	      with-exception-handler
	      call-with-values)
     (syntax: with-current-dynamic-environment
	      call/cc
	      call-with-values)
     (syntax: try
	      <condition>-predicate
	      not)
     (syntax: with-compensations/on-error
	      eq?
	      cons
	      run-compensations-store
	      compensations)
     (syntax: with-compensations
	      eq?
	      cons
	      run-compensations-store
	      compensations)
     (syntax: push-compensation
	      push-compensation-thunk)
     (syntax: with-compensation-handler
	      push-compensation-thunk)
     (syntax: compensate)
     ;;
     unwinding-call/cc
     mmck-return-handler)
  (import (scheme)
	  (mmck exceptional-conditions helpers)
	  (mmck exceptional-conditions condition-objects)
	  (mmck exceptional-conditions handlers)
	  (mmck exceptional-conditions compensations))
  (import-for-syntax (scheme)
		     (only (chicken base)
			   define-record)
		     (only (chicken syntax)
			   er-macro-transformer)
		     (only (matchable)
			   match)
		     (mmck exceptional-conditions helpers))


;;;; syntax XOR

(define-syntax xor
  (syntax-rules ()
    ((_ expr ...)
     (xor-aux #F expr ...))))

(define-syntax xor-aux
  (syntax-rules ()
    ((_ r)
     r)
    ((_ r expr)
     (let ((x expr))
       (if r
           (and (not x) r)
	 x)))
    ((_ r expr0 expr ...)
     (let ((x expr0))
       (and (or (not r) (not x))
	    (let ((n (or r x)))
	      (xor-aux n expr ...)))))))


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
  (apply syntax-error 'with-unwind-handler message input-form.stx args))

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
      (define %with-unwind-handler	(rename 'with-unwind-handler))
      (define %lambda			(rename 'lambda))
      (define %why			(rename 'why))
      (match input-form.stx
	((_ ?body ?cleanup0 ?cleanup* ...)
	 `(,%with-unwind-handler
	   (,%lambda (,%why) ,?cleanup0 . ,?cleanup*)
	   (,%lambda () ,?body)))
	(_
	 (syntax-error "invalid syntax in macro use"))))))


;;;; syntax GUARD
;;
;;The following implementation  of the GUARD syntax  is really sophisticated because it  has to deal
;;with both the dynamic environment requirements of R6RS and the unwind protection mechanism defined
;;by this package.   For a through explanation  we should read the documentation  in Texinfo format,
;;both the one of GUARD and the one of the unwind protection mechanism.
;;
;;
;;About the dynamic environment
;;-----------------------------
;;
;;In a syntax use like:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   ?expr2))
;;     ?body0 ?body ...)
;;
;;if the ?BODY raises  an exception: one of the clauses will certainly  be executed because there is
;;an  ELSE clause.   The  ?BODY  might mutate  the  dynamic environment;  all  the  ?TEST and  ?EXPR
;;expressions must be evaluated in the dynamic environment of the use of GUARD.
;;
;;In a syntax use like:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1))
;;     ?body0 ?body ...)
;;
;;if  all  the  ?TEST  expressions  evaluate  to   false:  we  must  re-raise  the  exception  using
;;RAISE-CONTINUABLE; so the syntax is "almost" equivalent to:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   (raise-continuable E)))
;;     ?body0 ?body ...)
;;
;;but: ?BODY  might mutate  the dynamic  environment; all the  ?TEST and  ?EXPR expressions  must be
;;evaluated in the dynamic environment of the use of GUARD; the RAISE-CONTINUABLE in the ELSE clause
;;must be evaluated the dynamic environment of the ?BODY.
;;
;;We must remember that, when using:
;;
;;   (with-exception-handler ?handler ?thunk)
;;
;;the ?HANDLER procedure is evaluated in the  dynamic environment of the ?THUNK, minus the exception
;;handler itself.  So, in pseudo-code, a syntax use with ELSE clause must be expanded as follows:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   ?expr2))
;;     ?body0 ?body ...)
;;   ==> (save-guard-continuation
;;        (with-exception-handler
;;            (lambda (E)
;;              (reinstate-guard-continuation
;;               (cond (?test0 ?expr0)
;;                     (?test1 ?expr1)
;;                     (else   ?expr2))))
;;          (lambda () ?body0 ?body ...)))
;;
;;and, also in pseudo-code, a syntax use without ELSE clause must be expanded as follows:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1))
;;     ?body0 ?body ...)
;;   ==> (save-guard-continuation
;;        (with-exception-handler
;;            (lambda (E)
;;              (save-exception-handler-continuation
;;               (reinstate-guard-continuation
;;                (cond (?test0 ?expr0)
;;                      (?test1 ?expr1)
;;                      (else   (reinstate-exception-handler-continuation
;;                               (raise-continuable E)))))))
;;          (lambda () ?body0 ?body ...)))
;;
;;notice how, in  the exception handler, we have to  jump out and in the dynamic  environment of the
;;exception handler itself.
;;
;;
;;About the unwind-protection mechanism
;;-------------------------------------
;;
;;There is some serious shit going on here  to support the unwind-protection mechanism as defined by
;;this package; let's focus on unwind-proteciton in the case of raised exception.  When using:
;;
;;   (with-unwind-handler ?cleanup ?thunk)
;;
;;the ?CLEANUP is associated to the dynamic extent of the call to ?THUNK: when the dynamic extent is
;;terminated   (as   defined  by   this   package)   the  ?CLEANUP   is   called.    If  the   value
;;RUN-UNWIND-PROTECTION-CLEANUP-UPON-EXIT?   is set  to true  and the  dynamic extent  of a  call to
;;?THUNK is exited: the dynamic extent is considered terminated and ?CLEANUP is called.
;;
;;This package  defines as termination event  of a GUARD's ?BODY  the execution of a  GUARD's clause
;;that does not re-raise the exception.  For a GUARD use like:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   ?expr2))
;;     ?body0 ?body ...)
;;
;;we can imagine the pseudo-code:
;;
;;   (guard (E (?test0 (run-unwind-protection-cleanups) ?expr0)
;;             (?test1 (run-unwind-protection-cleanups) ?expr1)
;;             (else   (run-unwind-protection-cleanups) ?expr2))
;;     ?body0 ?body ...)
;;
;;and for a GUARD use like:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1))
;;     ?body0 ?body ...)
;;
;;we can imagine the pseudo-code:
;;
;;   (guard (E (?test0 (run-unwind-protection-cleanups) ?expr0)
;;             (?test1 (run-unwind-protection-cleanups) ?expr1)
;;             (else   (raise-continuable E)))
;;     ?body0 ?body ...)
;;
;;By doing things  this way: an exception  raised by an ?EXPR  does not impede the  execution of the
;;cleanups.  If a  ?TEST raises an exception the cleanups  will not be run, and there  is nothing we
;;can do about  it; ?TEST expressions are  usually calls to predicates that  recognise the condition
;;type of E, so the risk of error is reduced.
;;
;;So, in pseudo-code, a syntax use with ELSE clause must be expanded as follows:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   ?expr2))
;;     ?body0 ?body ...)
;;   ==> (save-guard-continuation
;;        (with-exception-handler
;;            (lambda (E)
;;              (reinstate-guard-continuation
;;               (cond (?test0 (run-unwind-protection-cleanups) ?expr0)
;;                     (?test1 (run-unwind-protection-cleanups) ?expr1)
;;                     (else   (run-unwind-protection-cleanups) ?expr2))))
;;          (lambda () ?body0 ?body ...)))
;;
;;and, also in pseudo-code, a syntax use without ELSE clause must be expanded as follows:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1))
;;     ?body0 ?body ...)
;;   ==> (save-guard-continuation
;;        (with-exception-handler
;;            (lambda (E)
;;              (save-exception-handler-continuation
;;               (reinstate-guard-continuation
;;                (cond (?test0 (run-unwind-protection-cleanups) ?expr0)
;;                      (?test1 (run-unwind-protection-cleanups) ?expr1)
;;                      (else   (reinstate-exception-handler-continuation
;;                               (raise-continuable E)))))))
;;          (lambda () ?body0 ?body ...)))
;;
;;But how is RUN-UNWIND-PROTECTION-CLEANUPS implemented?  To cause the cleanups to be called we must
;;set  to true  the  value RUN-UNWIND-PROTECTION-CLEANUP-UPON-EXIT?,  then cause  an  exit from  the
;;dynamic extent of the ?THUNKs.  The latter is a sophisticated operation implemented as follows:
;;
;;   (define (run-unwind-protection-cleanups)
;;     (run-unwind-protection-cleanup-upon-exit? #t)
;;     (save-clause-expression-continuation
;;      (reinstate-exception-handler-continuation
;;       (reinstate-clause-expression-continuation))))
;;
;;we jump in GUARD's exception handler dynamic  environment then immediately jump out in the GUARD's
;;clause expression dynamic environment.  Fucking weird...
;;
;;
;;Expansion example: GUARD with no ELSE clause
;;--------------------------------------------
;;
;;A syntax without else clause like looks like this:
;;
;;   (guard (E
;;           (?test0 ?expr0)
;;           (?test1 ?expr1)))
;;     ?body0 ?body ...)
;;
;;is expanded to:
;;
;;   ((call/cc
;;        (lambda (reinstate-guard-continuation)
;;          (lambda ()
;;            (with-exception-handler
;;                (lambda (raised-obj)
;;                  (let ((E raised-obj))
;;                    ((call/cc
;;                         (lambda (reinstate-exception-handler-continuation)
;;                           (reinstate-guard-continuation
;;                            (lambda ()
;;                              (define (run-unwind-protect-cleanups)
;;                                (run-unwind-protection-cleanup-upon-exit? 'exception)
;;                                (call/cc
;;                                    (lambda (reinstate-clause-expression-continuation)
;;                                      (reinstate-exception-handler-continuation
;;                                       (lambda ()
;;                                         (reinstate-clause-expression-continuation)))))
;;                                (run-unwind-protection-cleanup-upon-exit? #f))
;;                              (if ?test0
;;                                  (begin
;;                                    (run-unwind-protect-cleanups)
;;                                    ?expr0)
;;                                (if ?test1
;;                                    (begin
;;                                      (run-unwind-protect-cleanups)
;;                                      ?expr1)
;;                                  (reinstate-exception-handler-continuation
;;                                   (lambda ()
;;                                     (raise-continuable raised-obj))))))))))))
;;              (lambda ()
;;                ?body0 ?body ...))))))
;;
(define-syntax guard
  (er-macro-transformer
    (lambda (input-form.stx rename compare)


;;;; syntax GUARD: main function and helpers

(define (main input-form.stx)
  (receive (variable.id clauses.stx body.stx)
      (parse-input-form input-form.stx)
    (let ((run-clauses.stx (gen-clauses clauses.stx)))
      (build-output-form variable.id run-clauses.stx body.stx))))

(define (synner message . args)
  (apply syntax-error 'guard message input-form.stx args))

(define (check-variable variable.id)
  (unless (syntactic-identifier? variable.id)
    (synner "expected syntactic identifier as variable name" variable.id)))

(define (syntactic-identifier? obj)
  (symbol? obj))


;;;; syntax GUARD: syntactic identifiers needed in the output form

(define %begin		(rename 'begin))
(define %define		(rename 'define))
(define %lambda		(rename 'lambda))
(define %if		(rename 'if))
(define %let		(rename 'let))
(define %set!		(rename 'set!))

;;This is only for debugging purposes.
(define %debug-print	(rename 'debug-print))

(define %call/cc					(rename 'call/cc))
(define %raise-continuable				(rename 'raise-continuable))
(define %run-unwind-protection-cleanup-upon-exit?	(rename 'run-unwind-protection-cleanup-upon-exit?))
(define %with-exception-handler				(rename 'with-exception-handler))

;;; --------------------------------------------------------------------

(define %else				(rename 'else))
(define (is-else? obj)
  (compare obj %else))


;;;; syntax GUARD: parsing input form

(define (parse-input-form input-form.stx)
  (match input-form.stx
    ((_ (?variable ?clause* ...) ?body ?body* ...)
     (begin
       (check-variable ?variable)
       (values ?variable ?clause* (cons ?body ?body*))))
    (_
     (synner "invalid syntax in macro use"))))


;;;; syntax GUARD: building output form

(define (build-output-form variable.id run-clauses.stx body.stx)
  `((,%call/cc
     (,%lambda (reinstate-guard-continuation)
	       (,%lambda ()
			 (,%with-exception-handler
			  (,%lambda (raised-obj)
				    ;;If we  raise an  exception from  a DYNAMIC-WIND's  in-guard or
				    ;;out-guard while  trying to call  the cleanups: we reset  it to
				    ;;avoid leaving it true.
				    (,%run-unwind-protection-cleanup-upon-exit? #f)
				    (,%let ((,variable.id raised-obj))
					   ,run-clauses.stx))
			  ;;Here we use LAMBDA to propagate the type of the last body form.
			  (,%lambda () ,@body.stx)))))))


;;;; syntax GUARD:

(define (gen-clauses clause*)
  (let ((code-stx (%process-multi-cond-clauses clause*)))
    `((,%call/cc
       (,%lambda (reinstate-exception-handler-continuation)
		 (reinstate-guard-continuation
		  (,%lambda ()
			    (,%define (run-unwind-protect-cleanups)
				      ;;If this function  is called: a test in  the clauses returned
				      ;;non-false and the execution flow  is at the beginning of the
				      ;;corresponding clause expression.
				      ;;
				      ;;Reinstate the continuation of  the guard's exception handler
				      ;;and then immediately reinstate  this continuation: jump out,
				      ;;then jump back  in.  This causes the  dynamic environment of
				      ;;the   exception   handler   to  be   reinstated,   and   the
				      ;;unwind-protection cleanups are called.
				      ;;
				      ;;Yes,  we  must  really  set  the  parameter  to  the  symbol
				      ;;"exception"; this symbol is used  as argument for the unwind
				      ;;handlers.
				      (,%run-unwind-protection-cleanup-upon-exit? 'exception)
				      (,%call/cc
				       (,%lambda (reinstate-clause-expression-continuation)
						 (reinstate-exception-handler-continuation
						  (,%lambda ()
							    ;;CHICKEN wants this  escape function to
							    ;;return  one value!   It  is a  CHICKEN
							    ;;bug/limitation.  See  CHICKEN's issues
							    ;;repository.
							    (reinstate-clause-expression-continuation #f)))))
				      (,%run-unwind-protection-cleanup-upon-exit? #f))
			    ,code-stx)))))))

(define (%process-multi-cond-clauses clause*)
  (match clause*
    ;;There is no ELSE clause: insert code that reinstates the continuation of the exception handler
    ;;introduced by GUARD and re-raises the exception.
    (()
     `(reinstate-exception-handler-continuation
       (,%lambda ()
		 (,%raise-continuable raised-obj))))

    ;;There is an ELSE clause: no need to jump back to the exception handler introduced by GUARD.
    ((((? is-else?) ?else-body ?else-body* ...))
     `(,%begin
       (run-unwind-protect-cleanups)
       ,?else-body . ,?else-body*))

    ((?clause . ?clause*)
     (let ((kont-code.stx (%process-multi-cond-clauses ?clause*)))
       (%process-single-cond-clause ?clause kont-code.stx)))

    (?others
     (synner "invalid guard clause" ?others))))

(define (%process-single-cond-clause clause kont-code-stx)
  (match clause
    ((?test '=> ?proc)
     (let ((tmp (gensym)))
       `(,%let ((,tmp ,?test))
	       (,%if ,tmp
		     (,%begin
		      (run-unwind-protect-cleanups)
		      (,?proc ,tmp))
		     ,kont-code-stx))))

    ((?test)
     (let ((tmp (gensym)))
       `(,%let ((,tmp ,?test))
	       (,%if ,tmp
		     (,%begin
		      (run-unwind-protect-cleanups)
		      ,tmp)
		     ,kont-code-stx))))

    ((?test ?expr ?expr* ...)
     `(,%if ,?test
	    (,%begin
	     (run-unwind-protect-cleanups)
	     ,?expr . ,?expr*)
	    ,kont-code-stx))

    (_
     (synner "invalid guard clause" clause))))


;;;; syntax GUARD: let's go

(main input-form.stx))))


;;;; syntax WITH-BLOCKED-EXCEPTIONS

(define-syntax with-blocked-exceptions
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define %call/cc			(rename 'call/cc))
      (define %with-exception-handler	(rename 'with-exception-handler))
      (define %call-with-values		(rename 'call-with-values))
      (define %lambda			(rename 'lambda))

      (match input-form.stx
	((_ ?exception-retvals-maker ?thunk)
	 `(,%call/cc
	   (,%lambda (reinstate-with-blocked-exceptions-continuation)
		     (,%with-exception-handler
		      (,%lambda (E)
			   (,%call-with-values
			    (,%lambda ()
				 (,?exception-retvals-maker E))
			    reinstate-with-blocked-exceptions-continuation))
		      ,?thunk))))

	((_ ?thunk)
	 `(,%call/cc
	   (,%lambda (reinstate-with-blocked-exceptions-continuation)
		(,%with-exception-handler
		 reinstate-with-blocked-exceptions-continuation
		 ,?thunk))))

	(_
	 (syntax-error 'with-blocked-exceptions "invalid syntax in macro use"))))))


;;;; syntax WITH-CURRENT-DYNAMIC-ENVIRONMENT

(define-syntax with-current-dynamic-environment
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define %call/cc			(rename 'call/cc))
      (define %with-blocked-exceptions	(rename 'with-blocked-exceptions))
      (define %call-with-values		(rename 'call-with-values))
      (define %lambda			(rename 'lambda))

      (match input-form.stx
	((_ ?exception-retvals-maker ?thunk)
	 `(,%call/cc
	   (,%lambda (return-thunk-with-packed-environment)
		((,%call/cc
		  (,%lambda (reinstate-target-environment-continuation)
		       (return-thunk-with-packed-environment
			(,%lambda ()
			     (,%call/cc
			      (,%lambda (reinstate-thunk-call-continuation)
				   (reinstate-target-environment-continuation
				    (,%lambda ()
					 (,%call-with-values
					  (,%lambda ()
					       (,%with-blocked-exceptions
						,?exception-retvals-maker
						,?thunk))
					  reinstate-thunk-call-continuation)))))))))))))
	(_
	 (syntax-error 'with-current-dynamic-environment "invalid syntax in macro use"))))))


;;;; syntax TRY

(define-syntax try
  (er-macro-transformer
    (lambda (input-form.stx rename compare)


;;;; syntax TRY:helpers

(define (synner message . args)
  (apply syntax-error 'try message input-form.stx args))

(define (check-variable var-id)
  (unless (syntactic-identifier? var-id)
    (synner "expected identifier as variable" var-id)))

(define (syntactic-identifier? obj)
  (symbol? obj))


;;;; syntax TRY: syntactic identifiers needed in the output form

(define %lambda				(rename 'lambda))
(define %guard				(rename 'guard))
(define %with-unwind-handler		(rename 'with-unwind-handler))

(define %<condition>-predicate		(rename '<condition>-predicate))

(define %and				(rename 'and))
(define %or				(rename 'or))
(define %xor				(rename 'xor))
(define %not				(rename 'not))

;;; --------------------------------------------------------------------

(define %else				(rename 'else))
(define (is-else? obj)
  (compare obj %else))


;;;; syntax TRY: parsing input form

(define (main input-form.stx)
  (match input-form.stx
    ;;Full syntax.
    ((_ ?body ('catch ?var ?catch-clause0 ?catch-clause* ...) ('finally ?finally-body0 ?finally-body* ...))
     (begin
       (check-variable ?var)
       (let ((GUARD-CLAUSE* (parse-multiple-catch-clauses ?var (cons ?catch-clause0 ?catch-clause*)))
	     (why           (gensym)))
	 `(,%with-unwind-handler
	   (,%lambda (,why)
		,?finally-body0 . ,?finally-body*)
	   (,%lambda ()
		(,%guard (,?var . ,GUARD-CLAUSE*)
			 ,?body))))))

    ;;Only catch, no finally.
    ((_ ?body ('catch ?var ?catch-clause0 ?catch-clause* ...))
     (begin
       (check-variable ?var)
       (let ((GUARD-CLAUSE* (parse-multiple-catch-clauses ?var (cons ?catch-clause0 ?catch-clause*))))
	 `(,%guard (,?var . ,GUARD-CLAUSE*) ,?body))))

    ((_ ?body ('finally ?finally-body0 ?finally-body* ...))
     (let ((why (gensym)))
       `(,%with-unwind-handler
	 (,%lambda (,why)
	      ,?finally-body0 . ,?finally-body*)
	 (,%lambda ()
	      ,?body))))

    (_
     (synner "invalid syntax in macro use"))))

(define (parse-multiple-catch-clauses var-id clauses-stx)
  (match clauses-stx
    ;;Match when there is no ELSE clause.  Remember that GUARD will reraise the exception when there
    ;;is no ELSE clause.
    (()
     '())

    ;;This branch with  the ELSE clause must come first!!!   The ELSE clause is valid only  if it is
    ;;the last.
    ((((? is-else?) ?else-body0 ?else-body ...))
     clauses-stx)

    (((?pred ?tag-body0 ?tag-body* ...) . ?other-clauses)
     (cons (cons* (match ?pred
		    (((? syntactic-identifier? ?tag))
		     `((,%<condition>-predicate ,?tag) ,var-id))
		    (_
		     (parse-logic-predicate-syntax ?pred
						   (lambda (tag-id)
						     (match tag-id
						       ((? syntactic-identifier? ?tag)
							`((,%<condition>-predicate ,?tag) ,var-id))
						       (_
							(synner "expected identifier as condition kind" tag-id)))))))
		  ?tag-body0 ?tag-body*)
	   (parse-multiple-catch-clauses var-id ?other-clauses)))

    ((?clause . ?other-clauses)
     (synner "invalid catch clause in try syntax" ?clause))))

(case-define parse-logic-predicate-syntax
  ;;Given a syntax object STX parse it as logic predicate expression with expected format:
  ;;
  ;;   STX = (and ?expr0 ?expr ...)
  ;;       | (or  ?expr0 ?expr ...)
  ;;       | (xor ?expr0 ?expr ...)
  ;;       | (not ?expr)
  ;;       | ?expr
  ;;
  ;;where AND,  OR, XOR,  NOT matched by  symbol name.  If  a standalone  ?EXPR is found:  apply the
  ;;procedure TAIL-PROC  to it gather  its single return value;  TAIL-PROC defaults to  the identity
  ;;function.
  ;;
  ;;Return a syntax object representing the logic predicate with the standalone expressions replaced
  ;;by the return values of TAIL-PROC.
  ;;
  ((stx)
   (parse-logic-predicate-syntax stx (lambda (stx) stx)))
  ((stx tail-proc)
   (define (recurse expr)
     (parse-logic-predicate-syntax expr tail-proc))
   (match stx
     (('and ?expr0 ?expr* ...)
      `(,%and ,@(map recurse (cons ?expr0 ?expr*))))
     (('or  ?expr0 ?expr* ...)
      `(,%or  ,@(map recurse (cons ?expr0 ?expr*))))
     (('xor ?expr0 ?expr* ...)
      `(,%xor ,@(map recurse (cons ?expr0 ?expr*))))
     (('not ?expr)
      `(,%not ,(recurse ?expr)))
     (_
      (tail-proc stx)))))


;;;; syntax TRY: let's go

(main input-form.stx))))


;;;; compensations

(define-syntax with-compensations/on-error
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define (synner message . args)
	(apply syntax-error 'with-compensations/on-error message input-form.stx args))

      (define %case-lambda		(rename 'case-lambda))
      (define %let			(rename 'let))
      (define %lambda			(rename 'lambda))
      (define %parameterize		(rename 'parameterize))
      (define %with-unwind-handler	(rename 'with-unwind-handler))
      (define %when			(rename 'when))
      (define %if			(rename 'if))
      (define %set!			(rename 'set!))
      (define %compensations		(rename 'compensations))
      (define %cons			(rename 'cons))
      (define %eq?			(rename 'eq?))
      (define %run-compensations-store	(rename 'run-compensations-store))

      (define (%make-store-binding store)
	(let ((stack        (gensym))
	      (false/thunk  (gensym)))
	  `((,store (,%let ((,stack '()))
			   (,%case-lambda
			    (()
			     ,stack)
			    ((,false/thunk)
			     (,%if ,false/thunk
				   (,%set! ,stack (,%cons ,false/thunk ,stack))
				   (,%set! ,stack '())))))))))

      (match input-form.stx
	((_ ?body0 ?body* ...)
	 (let ((store (gensym))
	       (why   (gensym)))
	   `(,%let ,(%make-store-binding store)
		   (,%parameterize ((,%compensations ,store))
				   (,%with-unwind-handler
				    (,%lambda (,why)
					 (,%when (,%eq? ,why 'exception)
						 (,%run-compensations-store ,store)))
				    (,%lambda ()
					 ,?body0 . ,?body*))))))
	(_
	 (synner "invalid syntax in macro use"))))))

;;; --------------------------------------------------------------------

(define-syntax with-compensations
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define (synner message . args)
	(apply syntax-error 'with-compensations message input-form.stx args))

      (define %case-lambda		(rename 'case-lambda))
      (define %let			(rename 'let))
      (define %lambda			(rename 'lambda))
      (define %parameterize		(rename 'parameterize))
      (define %with-unwind-handler	(rename 'with-unwind-handler))
      (define %if			(rename 'if))
      (define %set!			(rename 'set!))
      (define %compensations		(rename 'compensations))
      (define %cons			(rename 'cons))
      (define %eq?			(rename 'eq?))
      (define %run-compensations-store	(rename 'run-compensations-store))

      (define (%make-store-binding store)
	(let ((stack        (gensym))
	      (false/thunk  (gensym)))
	  `((,store (,%let ((,stack '()))
			   (,%case-lambda
			    (()
			     ,stack)
			    ((,false/thunk)
			     (,%if ,false/thunk
				   (,%set! ,stack (,%cons ,false/thunk ,stack))
				   (,%set! ,stack '())))))))))

      (match input-form.stx
	((_ ?body0 ?body* ...)
	 (let ((store (gensym))
	       (why   (gensym)))
	   `(,%let ,(%make-store-binding store)
		   (,%parameterize ((,%compensations ,store))
				   (,%with-unwind-handler
				    (,%lambda (,why)
					 (,%run-compensations-store ,store))
				    (,%lambda ()
					 ,?body0 . ,?body*))))))
	(_
	 (synner "invalid syntax in macro use"))))))

;;; --------------------------------------------------------------------

(define-syntax push-compensation
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define (synner message . args)
	(apply syntax-error 'push-compensations message input-form.stx args))

      (define %lambda			(rename 'lambda))
      (define %push-compensation-thunk	(rename 'push-compensation-thunk))

      (match input-form.stx
	((_ ?release0 ?release* ...)
	 `(,%push-compensation-thunk (,%lambda () ,?release0 ,@?release*)))

	(_
	 (synner "invalid syntax in macro use"))))))

;;; --------------------------------------------------------------------

(define-syntax with-compensation-handler
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define (synner message . args)
	(apply syntax-error 'with-compensation-handler message input-form.stx args))

      (define %begin			(rename 'begin))
      (define %push-compensation-thunk	(rename 'push-compensation-thunk))

      (match input-form.stx
	((_ ?release-thunk ?alloc-thunk)
	 `(,%begin
	    (,%push-compensation-thunk ,?release-thunk)
	    (,?alloc-thunk)))
	(_
	 (synner "invalid syntax in macro use"))))))

;;; --------------------------------------------------------------------

(define-syntax compensate
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (define (synner message . args)
	(apply syntax-error 'compensate message input-form.stx args))

      (define %push-compensation	(rename 'push-compensation))
      (define %receive-and-return	(rename 'receive-and-return))
      (define %begin			(rename 'begin))

      (match input-form.stx
	((_ ?alloc0 ?form* ...)
	 (let ((free #f))
	   (define alloc*
	     (let recur ((form-stx ?form*))
	       (match form-stx
		 ((('with ?release0 ?release* ...))
		  (begin
		    (set! free `(,%push-compensation ,?release0 ,@?release*))
		    '()))

		 (()
		  (synner "invalid compensation syntax: missing WITH keyword" form-stx))

		 ((('with))
		  (synner "invalid compensation syntax: empty WITH keyword"))

		 ((?alloc ?form* ...)
		  (cons ?alloc (recur ?form*)))
		 )))
	   (let ((the-obj (gensym)))
	     `(,%receive-and-return (,the-obj)
				    (,%begin ,?alloc0 . ,alloc*)
				    ,free))))

	(_
	 (synner "invalid syntax in macro use"))))))


;;;; done

#| end of module |# )

;;; end of file

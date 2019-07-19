;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for TRY
;;;Date: Jul 19, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the syntax TRY.
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

(module (test-try)
    ()
  (import (scheme)
	  (only (chicken format)
		format)
    (mmck exceptional-conditions)
    (mmck exceptional-conditions helpers)
    (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing TRY\n")


;;;; helpers



(parameterise ((check-test-name	'try))

  (let ()	;with else clause
    (define-condition-type &this
	&error
      make-this-condition
      condition-this?
      (a condition-this.a)
      (b condition-this.b)
      (c condition-this.c))

    (define (doit thunk)
      (try
	  (thunk)
	(catch E
	  ((&this)
	   (list (condition-this.a E)
		 (condition-this.b E)
		 (condition-this.c E)))
	  ((&message)
	   (condition-message E))
	  (else E))))

    (check
	(doit (lambda ()
		(raise (make-this-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 123)

    (check
	(try
	    (raise 123)
	  (catch E
	    ((&this)
	     (list (condition-this.a E)
		   (condition-this.b E)
		   (condition-this.c E)))
	    ((&message)
	     (condition-message E))
	    (else E)))
      => 123)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;with else clause
    (define-condition-type &that
	&error
      make-that-condition
      condition-that?
      (a condition-that.a)
      (b condition-that.b)
      (c condition-that.c))

    (define (doit thunk)
      (try
	  (thunk)
	(catch T
	  ((&that)
	   (list (condition-that.a T)
		 (condition-that.b T)
		 (condition-that.c T)))
	  ((&message)
	   (condition-message T))
	  (else T))))

    (check
	(doit (lambda ()
		(raise (make-that-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 123)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;without else clause
    (define-condition-type &those
	&error
      make-those-condition
      condition-those?
      (a condition-those.a)
      (b condition-those.b)
      (c condition-those.c))

    (define (doit thunk)
      (guard (E (else
		 (values 'reraised E)))
	(try
	    (thunk)
	  (catch E
	    ((&those)
	     (list (condition-those.a E)
		   (condition-those.b E)
		   (condition-those.c E)))
	    ((&message)
	     (condition-message E))))))

    (check
	(doit (lambda ()
		(raise (make-those-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 'reraised 123)

    #f)

;;; --------------------------------------------------------------------
;;; finally

  (check	;no exception
      (let ((a 1))
	(try
	    (set! a (+ a 10))
	  (catch E
	    ((&error)	E)
	    ((&warning)	E)
	    (else	E))
	  (finally
	   (set! a (+ a 100))))
	a)
    => 111)

  (check	;no exception
      (with-result
	(try
	    (add-result 'body)
	  (catch E
	    ((&error)	(add-result 'catch-error))
	    ((&warning)	(add-result 'catch-warning))
	    (else	(add-result 'catch-else)))
	  (finally
	   (add-result 'finally))))
    => '(body (body finally)))

  (check	;with exception
      (let ((a 1))
	(try
	    (raise (make-warning))
	  (catch E
	    ((&error)	E)
	    ((&warning)	(set! a (+ a 10)) E)
	    (else	E))
	  (finally
	   (set! a (+ a 100))))
	a)
    => 111)

  (check	;with exception
      (with-result
	(try
	    (begin
	      (add-result 'body)
	      (raise (make-warning)))
	  (catch E
	    ((&error)	(add-result 'catch-error))
	    ((&warning)	(add-result 'catch-warning))
	    (else	(add-result 'catch-else)))
	  (finally
	   (add-result 'finally))))
    => '(catch-warning (body catch-warning finally)))

;;; --------------------------------------------------------------------
;;; no catch clause

  (check	;normal exit
      (with-result
	(try
	    (begin
	      (add-result 'in)
	      1)
	  (finally
	   (add-result 'out))))
    => '(1 (in out)))

  #t)


(parameterise ((check-test-name	'basic-mechanism))

  (check	;normal exit
      (with-result
	(try
	    (begin
	      (add-result 'in)
	      1)
	  (catch E
	    (else
	     (add-result 'catch-else)))
	  (finally
	   (add-result 'out))))
    => '(1 (in out)))

  (check	;normal exit
      (with-result
	(receive-and-return (flag)
	    #f
	  (try
	      (add-result 'body)
	    (catch E
	      (else
	       (add-result 'catch-else)))
	    (finally
	     (add-result 'cleanup)
	     (set! flag #t)))))
    => '(#t (body cleanup)))

  (check	;multiple return values
      (with-result
	(receive (a b)
	    (try
		(begin
		  (add-result 'in)
		  (values 1 2))
	      (catch E
		(else
		 (add-result 'catch-else)))
	      (finally
	       (add-result 'out)))
	  (list a b)))
    => '((1 2) (in out)))

  (check	;zero return values
      (with-result
	(try
	    (begin
	      (add-result 'in)
	      (values))
	  (catch E
	    (else
	     (add-result 'catch-else)))
	  (finally
	   (add-result 'out))))
    => `((in out)))

  #f)


(parameterise ((check-test-name	'non-continuable-exceptions-from-body))

  (check	;show the mechanism of non-continuable exceptions
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape E))
	      (lambda ()
		(raise 1)))))
    => 1)

  (check	;show the mechanism of non-continuable exceptions
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		escape
	      (lambda ()
		(with-exception-handler
		    (lambda (E)
		      (raise E))
		  (lambda ()
		    (raise 1)))))))
    => 1)

;;; --------------------------------------------------------------------

  (check 	;exception in body, GUARD's ELSE clause
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (try
	      (begin
		(add-result 'thunk-in)
		(raise 2)
		(add-result 'thunk-out)
		1)
	    (catch E
	      ((&condition)
	       (add-result 'catch-condition)))
	    (finally
	     (add-result 'cleanup)))))
    => '(2 (thunk-in cleanup guard-else)))

  (check 	;exception in body, GUARD's clause
      (with-result
	(guard (E ((begin
		     (add-result 'guard-test)
		     #t)
		   (add-result 'guard-expr)
		   E))
	  (try
	      (begin
		(add-result 'thunk-in)
		(raise 2)
		(add-result 'thunk-out)
		1)
	    (catch E
	      ((&condition)
	       (add-result 'catch-condition)))
	    (finally
	     (add-result 'cleanup)))))
    => '(2 (thunk-in guard-test cleanup guard-expr)))

  (check  	;exception in body, nested GUARD uses, nested DYNAMIC-WIND
      (with-result
	(guard (E ((begin
		     (add-result 'guard-outer-test)
		     #t)
		   (add-result 'guard-outer-expr)
		   E))
	  (guard (E ((begin
		       (add-result 'guard-inner-test)
		       #f)
		     (add-result 'guard-inner-expr)
		     E))
	    (dynamic-wind
		(lambda ()
		  (add-result 'outer-before))
		(lambda ()
		  (try
		      (begin
			(dynamic-wind
			    (lambda ()
			      (add-result 'inner-before))
			    (lambda ()
			      (add-result 'thunk-in)
			      (raise 2)
			      (add-result 'thunk-out)
			      1)
			    (lambda ()
			      (add-result 'inner-after))))
		    (catch E
		      ((&condition)
		       (add-result 'catch-condition)))
		    (finally
		     (add-result 'cleanup))))
		(lambda ()
		  (add-result 'outer-after))))))
    => '(2 (outer-before inner-before thunk-in inner-after
			 inner-before inner-after ;this is from the GUARD inside TRY
			 outer-after
			 guard-inner-test
			 outer-before inner-before inner-after outer-after
			 guard-outer-test
			 outer-before inner-before inner-after cleanup outer-after
			 guard-outer-expr)))

;;; --------------------------------------------------------------------
;;; exiting the dynamic extent of ?BODY

  ;;Exit the dynamic  extent of ?BODY by reinstating an  escape continuation from an
  ;;exception handler.  The ?CLEANUP is not called.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'exception-handler)
		    (escape E))
		(lambda ()
		  (try
		      (begin
			(add-result 'thunk-in)
			(raise 123)
			(add-result 'thunk-out))
		    (catch E
		      ((&condition)
		       (add-result 'catch-condition)))
		    (finally
		     (add-result 'cleanup))))))))
    => '(123 (thunk-in exception-handler)))

  ;;Exit the dynamic  extent of ?BODY by reinstating an  escape continuation from an
  ;;exception handler.  The ?CLEANUP is not called.
  ;;
  (check
      (with-result
	(receive (rv)
	    (call/cc
		(lambda (escape)
		  (with-exception-handler
		      (lambda (E)
			(add-result 'exception-handler)
			(escape (lambda ()
				  (add-result 'after-escape)
				  E)))
		    (lambda ()
		      (try
			  (begin
			    (add-result 'thunk-in)
			    (raise 123)
			    (add-result 'thunk-out))
			(catch E
			  ((&condition)
			   (add-result 'catch-condition)))
			(finally
			 (add-result 'cleanup)))))))
	  (rv)))
    => '(123 (thunk-in exception-handler after-escape)))

  ;;Exit the dynamic extent of ?BODY  by reinstating an escape continuation from the
  ;;internals of a GUARD clause.
  ;;
  (check
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (try
	      (begin
		(add-result 'thunk-in)
		(raise 123)
		(add-result 'thunk-out))
	    (catch E
	      ((&condition)
	       (add-result 'catch-condition)))
	    (finally
	     (add-result 'cleanup)))))
    => '(123 (thunk-in cleanup guard-else)))

;;; --------------------------------------------------------------------
;;; raising non-continuable exceptions and DYNAMIC-WIND

  ;;The ?CLEANUP is not called.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'exception-handler)
		    (escape E))
		(lambda ()
		  (try
		      (begin
			(dynamic-wind
			    (lambda ()
			      (add-result 'in-guard))
			    (lambda ()
			      (add-result 'thunk-in)
			      (raise 123)
			      (add-result 'thunk-out))
			    (lambda ()
			      (add-result 'out-guard))))
		    (catch E
		      ((&condition)
		       (add-result 'catch-condition)))
		    (finally
		     (add-result 'cleanup))))))))
    => '(123 (in-guard thunk-in
		       ;;We  go  out  then  back  in because  the  GUARD  inside  TRY
		       ;;re-raises the exception with RAISE-CONTINUABLE.
		       out-guard in-guard
		       exception-handler out-guard)))

  #f)


(parameterise ((check-test-name	'continuable-exceptions-from-thunk))

  (check	;show the mechanism of continuable exceptions
      (with-exception-handler
	  (lambda (E)
	    (+ E 2))
	(lambda ()
	  (raise-continuable 1)))
    => 3)

;;; --------------------------------------------------------------------

  (check	;continuable exception from the body
      (with-result
	(with-exception-handler
	    (lambda (E)
	      (add-result 'exception-handler)
	      (+ E 2))
	  (lambda ()
	    (try
		(begin
		  (add-result 'thunk-in)
		  (begin0
		      (raise-continuable 1)
		    (add-result 'thunk-out)))
	      (catch E
		((&condition)
		 (add-result 'catch-condition)))
	      (finally
	       (add-result 'cleanup))))))
    => '(3 (thunk-in exception-handler thunk-out cleanup)))

  (check	;continuable exception from the body
      (with-result
	(guard (E ((non-continuable-violation? E)
		   99)
		  (else E))
	  (with-exception-handler
	      (lambda (E)
		(add-result 'exception-handler)
		(+ E 2))
	    (lambda ()
	      (try
		  (begin
		    (add-result 'thunk-in)
		    (begin0
			(raise-continuable 1)
		      (add-result 'thunk-out)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'cleanup)))))))
    => '(3 (thunk-in exception-handler thunk-out cleanup)))

;;; --------------------------------------------------------------------
;;; exiting the dynamic extent of ?BODY

  ;;Exit the dynamic  extent of ?BODY by reinstating an  escape continuation from an
  ;;exception handler.  ?CLEANUP is not called.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'exception-handler)
		    (escape E))
		(lambda ()
		  (try
		      (begin
			(add-result 'thunk-in)
			(raise-continuable 123)
			(add-result 'thunk-out))
		    (catch E
		      ((&condition)
		       (add-result 'catch-condition)))
		    (finally
		     (add-result 'cleanup))))))))
    => '(123 (thunk-in exception-handler)))

  ;;Exit the dynamic  extent of ?BODY by reinstating an  escape continuation from an
  ;;exception handler.  ?CLEANUP is not called.
  (check
      (with-result
	(receive (rv)
	    (call/cc
		(lambda (escape)
		  (with-exception-handler
		      (lambda (E)
			(add-result 'exception-handler)
			(escape (lambda ()
				  (add-result 'after-escape)
				  E)))
		    (lambda ()
		      (try
			  (begin
			    (add-result 'thunk-in)
			    (raise-continuable 123)
			    (add-result 'thunk-out))
			(catch E
			  ((&condition)
			   (add-result 'catch-condition)))
			(finally
			 (add-result 'cleanup)))))))
	  (rv)))
    => '(123 (thunk-in exception-handler after-escape)))

  ;;Exit the dynamic extent of ?BODY  by reinstating an escape continuation from the
  ;;ELSE GUARD clause.
  ;;
  (check
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (try
	      (begin
		(add-result 'thunk-in)
		(raise-continuable 123)
		(add-result 'thunk-out))
	    (catch E
	      ((&condition)
	       (add-result 'catch-condition)))
	    (finally
	     (add-result 'cleanup)))))
    => '(123 (thunk-in cleanup guard-else)))

  ;;Raise a continuable  exception in ?BODY; go  through a GUARD with  no ELSE, which
  ;;reraises  the continuable  exception;  execute an  exception  handler; return  to
  ;;?body; perform normal return.
  ;;
  (check
      (with-result
	(with-exception-handler
	    (lambda (E)
	      (add-result 'exception-handler)
	      (+ 2 E))
	  (lambda ()
	    (guard (E ((error? E)
		       (add-result 'guard-error)
		       E))
	      (try
		  (begin
		    (add-result 'thunk-in)
		    (begin0
			(raise-continuable 1)
		      (add-result 'thunk-out)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'cleanup)))))))
    => '(3 (thunk-in exception-handler thunk-out cleanup)))

;;; --------------------------------------------------------------------
;;; raising continuable exceptions and DYNAMIC-WIND

  (check	;cleanup not called
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'exception-handler)
		    (escape E))
		(lambda ()
		  (try
		      (begin
			(dynamic-wind
			    (lambda ()
			      (add-result 'in-guard))
			    (lambda ()
			      (add-result 'thunk-in)
			      (raise-continuable 123)
			      (add-result 'thunk-out))
			    (lambda ()
			      (add-result 'out-guard))))
		    (catch E
		      ((&condition)
		       (add-result 'catch-condition)))
		    (finally
		     (add-result 'cleanup))))))))
    => '(123 (in-guard thunk-in
		       ;;We  go  out  then  back  in because  the  GUARD  inside  TRY
		       ;;re-raises the exception with RAISE-CONTINUABLE.
		       out-guard in-guard
		       exception-handler out-guard)))

  #f)


(parameterise ((check-test-name	'exceptions-from-cleanup))

  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		escape
	      (lambda ()
		(with-exception-handler
		    (lambda (E)
		      (raise 2))
		  (lambda ()
		    (raise 1)))))))
    => 2)

;;; --------------------------------------------------------------------

  (check	;the exception in the cleanup is discarded
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (try
	      (begin
		(add-result 'thunk-in)
		(raise 1)
		(add-result 'thunk-out))
	    (catch E
	      ((&condition)
	       (add-result 'catch-condition)))
	    (finally
	     (add-result 'cleanup-in)
	     (raise 2)
	     (add-result 'cleanup-out)))))
    => '(1 (thunk-in cleanup-in guard-else)))

  #f)


(parameterise ((check-test-name	'non-local-exit-with-return))

  (check	;return in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (returnable
	    (try
		(begin
		  (add-result 'thunk-in)
		  (return 123)
		  (add-result 'thunk-out))
	      (catch E
		((&condition)
		 (add-result 'catch-condition)))
	      (finally
	       (add-result 'cleanup)
	       (set! flag #t))))))
    => '(#t (thunk-in cleanup)))

  (check	;return in body, documentation example
      (internal-body
	(define y #f)
	(define x
	  (returnable
	    (try
		(return 1)
	      (catch E
		((&condition)
		 (add-result 'catch-condition)))
	      (finally
	       (set! y #t)))))
	(values x y))
    => 1 #t)

  #f)


(parameterise ((check-test-name	'reentering-continuations))

  (check	;reentering continuation
      (with-result
	(guard (E ((non-reinstatable-violation? E)
		   (add-result 'violation)
		   #t)
		  (else E))
	  (let ((rv (try
			(begin
			  (add-result 'thunk-in)
			  (begin0
			      (call/cc values)
			    (add-result 'thunk-out)))
		      (catch E
			((&condition)
			 (add-result 'catch-condition)))
		      (finally
		       (add-result 'cleanup)))))
	    (cond ((procedure? rv)
		   (add-result 'reinstating)
		   (rv 123))
		  (else
		   (add-result 'returning)
		   rv)))))
    => '(#t (thunk-in thunk-out cleanup reinstating violation)))

  (check	;documentation example
      (internal-body
	(define order '())
	(define (add obj)
	  (set! order (cons obj order)))

	(define rv
	  (guard (E ((non-reinstatable-violation? E)
		     (add 'violation)
		     #t)
		    (else E))
	    (let ((rv (try
			  (begin
			    (add 'thunk-in)
			    (begin0
				(call/cc values)
			      (add 'thunk-out)))
			(catch E
			  ((&condition)
			   (add-result 'catch-condition)))
			(finally
			 (add 'cleanup)))))
	      (cond ((procedure? rv)
		     (add 'reinstating)
		     (rv 123))
		    (else
		     (add 'returning)
		     rv)))))
	(values rv (reverse order)))
    => #t '(thunk-in thunk-out cleanup reinstating violation))

  #f)


(parameterise ((check-test-name	'coroutines))

  (define (print template . args)
    (apply format (current-error-port) template args)
    (yield))

;;; --------------------------------------------------------------------

  (check
      (let ((a #f) (b #f) (c #f))
	(concurrently
	  (lambda ()
	    (try
		(begin
		  (set! a 1.1)
		  (print "unwind-protect sub 1.1: ~a\n" a)
		  (set! a 1.2)
		  (print "unwind-protect sub 1.2: ~a\n" a)
		  (set! a 1.3)
		  (print "unwind-protect sub 1.3: ~a\n" a))
	      (catch E
		((&condition)
		 (add-result 'catch-condition)))
	      (finally
	       (set! a 1.4))))
	  (lambda ()
	    (try
		(begin
		  (set! b 2.1)
		  (print "unwind-protect sub 2.1: ~a\n" b)
		  (set! b 2.2)
		  (print "unwind-protect sub 2.2: ~a\n" b)
		  (set! b 2.3)
		  (print "unwind-protect sub 2.3: ~a\n" b))
	      (catch E
		((&condition)
		 (add-result 'catch-condition)))
	      (finally
	       (set! b 2.4))))
	  (lambda ()
	    (try
		(begin
		  (set! c 3.1)
		  (print "unwind-protect sub 3.1: ~a\n" c)
		  (set! c 3.2)
		  (print "unwind-protect sub 3.2: ~a\n" c)
		  (set! c 3.3)
		  (print "unwind-protect sub 3.3: ~a\n" c))
	      (catch E
		((&condition)
		 (add-result 'catch-condition)))
	      (finally
	       (set! c 3.4)))))
	(values a b c))
    => 1.4 2.4 3.4)

  #t)


(parameterise ((check-test-name	'dynamic-environment))

  (define parm
    (make-parameter #f))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(parameterise ((parm 'parm))
	  (try
	      (begin
		(add-result 'thunk-in)
		(add-result (parm))
		(add-result 'thunk-out)
		1)
	    (catch E
	      ((&condition)
	       (add-result 'catch-condition)))
	    (finally
	     (add-result 'cleanup-in)
	     (add-result (parm))
	     (add-result 'cleanup-out)))))
    => '(1 (thunk-in parm thunk-out cleanup-in parm cleanup-out)))

  ;;Changing the environment inside ?BODY does not affect ?CLEANUP.
  ;;
  (check
      (with-result
	(parameterise ((parm 'outer-parm))
	  (try
	      (begin
		(parameterise ((parm 'inner-parm))
		  (add-result 'thunk-in)
		  (add-result (parm))
		  (add-result 'thunk-out)
		  1))
	    (catch E
	      ((&condition)
	       (add-result 'catch-condition)))
	    (finally
	     (add-result 'cleanup-in)
	     (add-result (parm))
	     (add-result 'cleanup-out)))))
    => '(1 (thunk-in inner-parm thunk-out cleanup-in outer-parm cleanup-out)))

  ;;Changing  the environment  inside ?BODY  does  not affect  ?CLEANUP.  Exit  with
  ;;RETURN.
  ;;
  (check
      (with-result
	(returnable
	  (parameterise ((parm 'outer-parm))
	    (try
		(parameterise ((parm 'inner-parm))
		  (add-result 'thunk-in)
		  (add-result (parm))
		  (return 2)
		  (add-result 'thunk-out)
		  1)
	      (catch E
		((&condition)
		 (add-result 'catch-condition)))
	      (finally
	       (add-result 'cleanup-in)
	       (add-result (parm))
	       (add-result 'cleanup-out))))))
    => '(2 (thunk-in inner-parm cleanup-in outer-parm cleanup-out)))

;;; --------------------------------------------------------------------
;;; exit with RETURN

  (check
      (with-result
	(parameterise ((parm 'outer-parm))
	  (returnable
	    (parameterise ((parm 'inner-parm))
	      (try
		  (begin
		    (add-result 'thunk-in)
		    (add-result (parm))
		    (return 2)
		    (add-result 'thunk-out)
		    1)
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'cleanup-in)
		 (add-result (parm))
		 (add-result 'cleanup-out)))))))
    => '(2 (thunk-in inner-parm cleanup-in inner-parm cleanup-out)))

;;; --------------------------------------------------------------------
;;; raising exception from thunk

  (check
      (with-result
	(parameterise ((parm 'outer-parm))
	  (guard (E ((begin
		       (add-result 'guard-test-in)
		       (add-result (parm))
		       (add-result 'guard-test-out)
		       #t)
		     (add-result 'guard-expr-in)
		     (add-result (parm))
		     (add-result 'guard-expr-out)
		     E))
	    (parameterise ((parm 'inner-parm))
	      (try
		  (begin
		    (add-result 'thunk-in)
		    (add-result (parm))
		    (raise 2)
		    (add-result 'thunk-out)
		    1)
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'cleanup-in)
		 (add-result (parm))
		 (add-result 'cleanup-out)))))))
    => '(2 (thunk-in inner-parm
		     guard-test-in outer-parm guard-test-out
		     cleanup-in inner-parm cleanup-out
		     guard-expr-in outer-parm guard-expr-out)))

  #t)


(parameterise ((check-test-name	'exceptions-from-guards))

;;;What  happens when  we  raise an  exception  from the  in-guard  and out-guard  of
;;;DYNAMIC-WIND?

  (check	;no exceptions raised
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk))
		      (lambda ()
			(add-result 'inner/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'inner/***cleanup***))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'middle/inner-in-guard))
		      (lambda ()
			(add-result 'middle/thunk-in)
			(inner)
			(add-result 'middle/thunk-out))
		      (lambda ()
			(add-result 'middle/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'middle/***cleanup***))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'outer/inner-in-guard))
		      (lambda ()
			(add-result 'outer/thunk-in)
			(middle)
			(add-result 'outer/thunk-out)
			1)
		      (lambda ()
			(add-result 'outer/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'outer/***cleanup***))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (outer))
    => '(1 (outer/outer-in-guard
	    outer/inner-in-guard
	    outer/thunk-in
	    middle/outer-in-guard
	    middle/inner-in-guard
	    middle/thunk-in
	    inner/outer-in-guard
	    inner/inner-in-guard
	    inner/thunk
	    inner/inner-out-guard
	    inner/***cleanup***
	    inner/outer-out-guard
	    middle/thunk-out
	    middle/inner-out-guard
	    middle/***cleanup***
	    middle/outer-out-guard
	    outer/thunk-out
	    outer/inner-out-guard
	    outer/***cleanup***
	    outer/outer-out-guard)))

;;; --------------------------------------------------------------------
;;; raise exception from inner thunk

  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk-in/raise)
			(raise 2)
			(add-result 'inner/thunk-out))
		      (lambda ()
			(add-result 'inner/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'inner/***cleanup***))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'middle/inner-in-guard))
		      (lambda ()
			(add-result 'middle/thunk-in)
			(inner)
			(add-result 'middle/thunk-out))
		      (lambda ()
			(add-result 'middle/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'middle/***cleanup***))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'outer/inner-in-guard))
		      (lambda ()
			(add-result 'outer/thunk-in)
			(middle)
			(add-result 'outer/thunk-out)
			1)
		      (lambda ()
			(add-result 'outer/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'outer/***cleanup***))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(2
	 (outer/outer-in-guard
	  outer/inner-in-guard outer/thunk-in
	  middle/outer-in-guard middle/inner-in-guard
	  middle/thunk-in inner/outer-in-guard
	  inner/inner-in-guard inner/thunk-in/raise
	  inner/inner-out-guard inner/inner-in-guard
	  inner/inner-out-guard inner/outer-out-guard
	  middle/inner-out-guard middle/inner-in-guard
	  inner/outer-in-guard inner/inner-in-guard
	  inner/inner-out-guard inner/outer-out-guard
	  middle/inner-out-guard middle/outer-out-guard
	  outer/inner-out-guard outer/inner-in-guard
	  middle/outer-in-guard middle/inner-in-guard
	  inner/outer-in-guard inner/inner-in-guard
	  inner/inner-out-guard inner/outer-out-guard
	  middle/inner-out-guard middle/outer-out-guard
	  outer/inner-out-guard outer/outer-out-guard guard-test
	  outer/outer-in-guard outer/inner-in-guard
	  middle/outer-in-guard middle/inner-in-guard
	  inner/outer-in-guard inner/inner-in-guard
	  inner/inner-out-guard inner/***cleanup***
	  inner/outer-out-guard middle/inner-out-guard
	  middle/***cleanup*** middle/outer-out-guard
	  outer/inner-out-guard outer/***cleanup***
	  outer/outer-out-guard guard-expr)))

;;; --------------------------------------------------------------------
;;; raise exception from middle/inner-out-guard

  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk))
		      (lambda ()
			(add-result 'inner/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'inner/***cleanup***))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'middle/inner-in-guard))
		      (lambda ()
			(add-result 'middle/thunk-in)
			(inner)
			(add-result 'middle/thunk-out))
		      (lambda ()
			(add-result 'middle/inner-out-guard/raise)
			(raise 2)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'middle/***cleanup***))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'outer/inner-in-guard))
		      (lambda ()
			(add-result 'outer/thunk-in)
			(middle)
			(add-result 'outer/thunk-out)
			1)
		      (lambda ()
			(add-result 'outer/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'outer/***cleanup***))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(2
	 (outer/outer-in-guard
	  outer/inner-in-guard outer/thunk-in
	  middle/outer-in-guard middle/inner-in-guard
	  middle/thunk-in inner/outer-in-guard
	  inner/inner-in-guard inner/thunk inner/inner-out-guard
	  inner/***cleanup*** inner/outer-out-guard
	  middle/thunk-out middle/inner-out-guard/raise
	  middle/outer-out-guard outer/inner-out-guard
	  outer/inner-in-guard middle/outer-in-guard
	  middle/outer-out-guard outer/inner-out-guard
	  outer/outer-out-guard guard-test outer/outer-in-guard
	  outer/inner-in-guard middle/outer-in-guard
	  middle/***cleanup*** middle/outer-out-guard
	  outer/inner-out-guard outer/***cleanup***
	  outer/outer-out-guard guard-expr)))

;;; --------------------------------------------------------------------
;;; raise exception from middle/outer-out-guard

  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk))
		      (lambda ()
			(add-result 'inner/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'inner/***cleanup***))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'middle/inner-in-guard))
		      (lambda ()
			(add-result 'middle/thunk-in)
			(inner)
			(add-result 'middle/thunk-out))
		      (lambda ()
			(add-result 'middle/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'middle/***cleanup***))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard/raise)
	      (raise 2))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'outer/inner-in-guard))
		      (lambda ()
			(add-result 'outer/thunk-in)
			(middle)
			(add-result 'outer/thunk-out)
			1)
		      (lambda ()
			(add-result 'outer/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'outer/***cleanup***))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(2
	 (outer/outer-in-guard
	  outer/inner-in-guard outer/thunk-in
	  middle/outer-in-guard middle/inner-in-guard
	  middle/thunk-in inner/outer-in-guard
	  inner/inner-in-guard inner/thunk inner/inner-out-guard
	  inner/***cleanup*** inner/outer-out-guard
	  middle/thunk-out middle/inner-out-guard
	  middle/***cleanup*** middle/outer-out-guard/raise
	  outer/inner-out-guard outer/inner-in-guard
	  outer/inner-out-guard outer/outer-out-guard guard-test
	  outer/outer-in-guard outer/inner-in-guard
	  outer/inner-out-guard outer/***cleanup***
	  outer/outer-out-guard guard-expr)))

;;; --------------------------------------------------------------------
;;; raise exception from inner/thunk and then from middle/inner-out-guard

  ;;The inner/cleanup is never called.
  ;;
  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk-in/raise)
			(raise 2)
			(add-result 'inner/thunk-out))
		      (lambda ()
			(add-result 'inner/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'inner/***cleanup***))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'middle/inner-in-guard))
		      (lambda ()
			(add-result 'middle/thunk-in)
			(inner)
			(add-result 'middle/thunk-out))
		      (lambda ()
			(add-result 'middle/inner-out-guard/raise)
			(raise 3)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'middle/***cleanup***))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'outer/inner-in-guard))
		      (lambda ()
			(add-result 'outer/thunk-in)
			(middle)
			(add-result 'outer/thunk-out)
			1)
		      (lambda ()
			(add-result 'outer/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'outer/***cleanup***))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(3
	 (outer/outer-in-guard
	  outer/inner-in-guard outer/thunk-in
	  middle/outer-in-guard middle/inner-in-guard
	  middle/thunk-in inner/outer-in-guard
	  inner/inner-in-guard inner/thunk-in/raise
	  inner/inner-out-guard inner/inner-in-guard
	  inner/inner-out-guard inner/outer-out-guard
	  middle/inner-out-guard/raise middle/outer-out-guard
	  outer/inner-out-guard outer/inner-in-guard
	  middle/outer-in-guard middle/outer-out-guard
	  outer/inner-out-guard outer/outer-out-guard guard-test
	  outer/outer-in-guard outer/inner-in-guard
	  middle/outer-in-guard middle/***cleanup***
	  middle/outer-out-guard outer/inner-out-guard
	  outer/***cleanup*** outer/outer-out-guard guard-expr)))

;;; --------------------------------------------------------------------
;;; raise exception from inner/thunk and then from middle/inner-out-guard 2nd time

  ;;All the cleanups are called.
  ;;
  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk-in/raise)
			(raise 2)
			(add-result 'inner/thunk-out))
		      (lambda ()
			(add-result 'inner/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'inner/***cleanup***))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(define counter 0)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'middle/inner-in-guard))
		      (lambda ()
			(add-result 'middle/thunk-in)
			(inner)
			(add-result 'middle/thunk-out))
		      (lambda ()
			(add-result 'middle/inner-out-guard/raise)
			(set! counter (+ 1 counter))
			(when (= 2 counter)
			  (raise 3))))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'middle/***cleanup***))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (try
		  (dynamic-wind
		      (lambda ()
			(add-result 'outer/inner-in-guard))
		      (lambda ()
			(add-result 'outer/thunk-in)
			(middle)
			(add-result 'outer/thunk-out)
			1)
		      (lambda ()
			(add-result 'outer/inner-out-guard)))
		(catch E
		  ((&condition)
		   (add-result 'catch-condition)))
		(finally
		 (add-result 'outer/***cleanup***))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(3
	 (outer/outer-in-guard
	  outer/inner-in-guard outer/thunk-in
	  middle/outer-in-guard middle/inner-in-guard
	  middle/thunk-in inner/outer-in-guard
	  inner/inner-in-guard inner/thunk-in/raise
	  inner/inner-out-guard inner/inner-in-guard
	  inner/inner-out-guard inner/outer-out-guard
	  middle/inner-out-guard/raise middle/inner-in-guard
	  inner/outer-in-guard inner/inner-in-guard
	  inner/inner-out-guard inner/outer-out-guard
	  middle/inner-out-guard/raise middle/outer-out-guard
	  outer/inner-out-guard outer/inner-in-guard
	  middle/outer-in-guard middle/outer-out-guard
	  outer/inner-out-guard outer/outer-out-guard guard-test
	  outer/outer-in-guard outer/inner-in-guard
	  middle/outer-in-guard middle/***cleanup***
	  middle/outer-out-guard outer/inner-out-guard
	  outer/***cleanup*** outer/outer-out-guard guard-expr)))

  #t)


(parameterise ((check-test-name		'logic-predicates))

;;; logic AND

  (check
      (try
	  (raise (condition (make-error)
			    (make-warning)))
	(catch E
	  ((and &error &warning)
	   #t)
	  (else
	   #f)))
    => #t)

;;; --------------------------------------------------------------------
;;; logic OR

  (check
      (try
	  (raise (make-error))
	(catch E
	  ((or &error &warning)
	   #t)
	  (else
	   #f)))
    => #t)

  (check
      (try
	  (raise (make-warning))
	(catch E
	  ((or &error &warning)
	   #t)
	  (else
	   #f)))
    => #t)

;;; --------------------------------------------------------------------
;;; logic XOR

  (check
      (try
	  (raise (make-error))
	(catch E
	  ((xor &error &warning)
	   #t)
	  (else
	   #f)))
    => #t)

  (check
      (try
	  (raise (make-warning))
	(catch E
	  ((xor &error &warning)
	   #t)
	  (else
	   #f)))
    => #t)

  (check
      (try
	  (raise (condition (make-error)
			    (make-warning)))
	(catch E
	  ((xor &error &warning)
	   #f)
	  (else
	   #t)))
    => #t)

;;; --------------------------------------------------------------------
;;; logic NOT

  (check
      (try
	  (raise (make-warning))
	(catch E
	  ((not &syntax)
	   #t)
	  (else
	   #f)))
    => #t)

  (values))


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file

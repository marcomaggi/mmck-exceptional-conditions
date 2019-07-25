;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: exceptional condition handlers
;;;Date: Jul 16, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the exceptional condition handlers.
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

(declare (unit mmck.exceptional-conditions.handlers)
	 (uses mmck.exceptional-conditions.condition-objects)
	 (uses mmck.exceptional-conditions.helpers)
	 (emit-import-library mmck.exceptional-conditions.handlers))

(module (mmck.exceptional-conditions.handlers)
    (raise
     raise-continuable
     with-exception-handler
     error
     assertion-violation
     (syntax: assert error)
     non-reinstatable-violation
     mmck-exceptional-conditions-setup-interoperability)
  (import (scheme)
	  (only (chicken format)
		format)
	  (prefix (only (chicken condition)
			with-exception-handler
			current-exception-handler
			get-condition-property)
		  chicken::)
	  (only (mmck exceptional-conditions helpers)
		  current-error-port
		  exit
		  make-parameter
		  parameterize
		  when
		  unless
		  debug-print)
	  (mmck exceptional-conditions condition-objects))
  (import (only (chicken module) reexport))
  (import-for-syntax (scheme)
		     (only (chicken base)
			   define-record)
		     (only (chicken syntax)
			   er-macro-transformer)
		     (only (matchable)
			   match)
		     (mmck exceptional-conditions helpers))


;;;; current exception handler

(define current-handlers
  (make-parameter
      (list
       (lambda (cnd)
	 (cond ((warning? cnd)
		;;When a  "&warning" is  raised with RAISE-CONTINUABLE:  we want to  go on  with the
		;;execution.
		(format (current-error-port)
		  "CHICKEN: uncaught warning: ~a: ~a\n"
		  (cond ((who-condition? cnd)
			 (condition-who cnd))
			((chicken::get-condition-property cnd 'exn 'location)
			 => (lambda (loc) loc))
			(else
			 "unknown location"))
		  (cond ((message-condition? cnd)
			 (condition-message cnd))
			((chicken::get-condition-property cnd 'exn 'message)
			 => (lambda (msg) msg))
			(else
			 "unkown exception cause")))
		(cond ((irritants-condition? cnd)
		       (format (current-error-port)
			 "\t&irritants: ~a\n" (condition-irritants cnd)))
		      ((chicken::get-condition-property cnd 'exn 'arguments)
		       => (lambda (args)
			    (format (current-error-port)
			      "\targuments: ~a\n" args)))
		      (else
		       (format (current-error-port)
			 "\traised object: ~a\n" cnd)))
		(values))
	       (else
		(format (current-error-port)
		  "CHICKEN: uncaught exception: ~a: ~a\n"
		  (cond ((who-condition? cnd)
			 (condition-who cnd))
			((chicken::get-condition-property cnd 'exn 'location)
			 => (lambda (loc) loc))
			(else
			 "unknown location"))
		  (cond ((message-condition? cnd)
			 (condition-message cnd))
			((chicken::get-condition-property cnd 'exn 'message)
			 => (lambda (msg) msg))
			(else
			 "unkown exception cause")))
		(cond ((irritants-condition? cnd)
		       (format (current-error-port)
			 "\t&irritants: ~a\n" (condition-irritants cnd)))
		      ((chicken::get-condition-property cnd 'exn 'arguments)
		       => (lambda (args)
			    (format (current-error-port)
			      "\targuments: ~a\n" args)))
		      (else
		       (format (current-error-port)
			 "\traised object: ~a\n" cnd)))
		(when (serious-condition? cnd)
		  (exit 255))
		(values))))
       (lambda (cnd)
	 (format (current-error-port)
	   "CHICKEN: uncaught exception: ~a: ~a\n"
	   (cond ((who-condition? cnd)
		  (condition-who cnd))
		 ((chicken::get-condition-property cnd 'exn 'location)
		  => (lambda (loc) loc))
		 (else
		  "unknown location"))
	   (cond ((message-condition? cnd)
		  (condition-message cnd))
		 ((chicken::get-condition-property cnd 'exn 'message)
		  => (lambda (msg) msg))
		 (else
		  "unkown exception cause")))
	 (cond ((irritants-condition? cnd)
		(format (current-error-port)
		  "\t&irritants: ~a\n" (condition-irritants cnd)))
	       ((chicken::get-condition-property cnd 'exn 'arguments)
		=> (lambda (args)
		     (format (current-error-port)
		       "\targuments: ~a\n" args)))
	       (else
		(format (current-error-port)
		  "\traised object: ~a\n" cnd)))
	 (exit 255))
       #| end of LIST |# )))

(define (with-exception-handler handler thunk)
  (unless (procedure? handler)
    (assertion-violation 'with-exception-handler "expected procedure as HANDLER argument" handler))
  (unless (procedure? thunk)
    (assertion-violation 'with-exception-handler "expected procedure as THUNK argument" thunk))
  (parameterize ((current-handlers (cons handler (current-handlers))))
    #;(debug-print 'with-exception-handler 'installed-handler handler)
    (thunk)))


;;;; raising exceptions

(define (raise obj)
  #;(debug-print 'raise 'enter obj)
  (let* ((handlers	(current-handlers))
	 (head		(car handlers))
	 (tail		(cdr handlers)))
    (parameterize ((current-handlers tail))
      #;(debug-print 'raise 'head-handler head)
      (head obj)
      #;(debug-print 'raise 'returned-from-head-handler obj)
      #;(debug-print 'raise 'next-handler (car (current-handlers)))
      (raise (condition
	       (make-non-continuable-violation)
	       (make-who-condition 'raise)
	       (make-message-condition "handler returned from non-continuable exception")
	       (make-irritants-condition (list obj)))))))

(define (raise-continuable obj)
  (let* ((handlers	(current-handlers))
	 (head		(car handlers))
	 (tail		(cdr handlers)))
    (parameterize ((current-handlers tail))
      (head obj))))


;;;; raising general exceptions

(define (error who message . irritants)
  ;;R6RS states that there must be an "&irritants" condition.
  ;;
  (unless (who-condition-value? who)
    (assertion-violation 'error "invalid argument WHO" who))
  (unless (message-condition-value? message)
    (assertion-violation 'error "invalid argument MESSAGE" message))
  (raise
   (if who
       (condition
	 (make-error)
	 (make-who-condition who)
	 (make-message-condition message)
	 (make-irritants-condition irritants))
     (condition
       (make-error)
       (make-message-condition message)
       (make-irritants-condition irritants)))))

(define (assertion-violation who message . irritants)
  ;;R6RS states that there must be an "&irritants" condition.
  ;;
  (unless (who-condition-value? who)
    (assertion-violation 'assertion-violation "invalid argument WHO" who))
  (unless (message-condition-value? message)
    (assertion-violation 'assertion-violation "invalid argument MESSAGE" message))
  (raise
   (if who
       (condition
	 (make-assertion-violation)
	 (make-who-condition who)
	 (make-message-condition message)
	 (make-irritants-condition irritants))
     (condition
       (make-assertion-violation)
       (make-message-condition message)
       (make-irritants-condition irritants)))))

(define-syntax assert
  (syntax-rules ()
    ((_ ?expr)
     (or ?expr
	 (assertion-violation 'assert "failed assertion" (quote ?expr))))
    ))


;;;; raising specific exceptions

(define (non-reinstatable-violation who message . irritants)
  (unless (who-condition-value? who)
    (assertion-violation 'non-reinstatable-exception "invalid argument WHO" who))
  (unless (message-condition-value? message)
    (assertion-violation 'non-reinstatable-exception "invalid argument MESSAGE" message))
  (raise
   (if who
       (condition (make-non-reinstatable-violation)
		  (make-who-condition		who)
		  (make-message-condition	message)
		  (make-irritants-condition	irritants))
     (condition (make-non-reinstatable-violation)
		(make-message-condition		message)
		(make-irritants-condition	irritants)))))


;;;; interoperability with CHICKEN

(define (mmck-exceptional-conditions-setup-interoperability)
  (chicken::current-exception-handler raise-continuable))


;;;; done

#| end of module |# )

;;; end of file

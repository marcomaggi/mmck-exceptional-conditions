;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for exception handlers
;;;Date: Jul 16, 2019
;;;
;;;Abstract
;;;
;;;	This program tests exception handlers.
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

(module (test-exception-handers)
    ()
  (import (scheme)
	  (only (chicken base)
		call/cc)
	  (mmck exceptional-conditions)
	  (mmck exceptional-conditions helpers)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing exception handlers\n")


;;;; helpers

(define-syntax values->list
  (syntax-rules ()
    ((_ ?expr)
     (call-with-values
	 (lambda ()
	   ?expr)
       (lambda args args)))))


(parameterise ((check-test-name		'with-exception-handler))

  ;;No exception return the return value of THUNK.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape 456))
	      (lambda ()
		123))))
    => 123)

  ;;No exception return the multiple return values of THUNK.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape 4 5 6))
	      (lambda ()
		(values 1 2 3)))))
    => 1 2 3)

  ;;No exception return the no values of THUNK.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (values->list
	     (with-exception-handler
		 (lambda (E)
		   (escape))
	       (lambda ()
		 (values))))))
    => '())

;;; --------------------------------------------------------------------

  ;;Catch exception and escape.
  ;;
  (check
      (call-with-current-continuation
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (list (condition-who E)
				(condition-message E)
				(condition-irritants E))))
	      (lambda ()
		(error 'me "the message" 1 2 3)))))
    => '(me "the message" (1 2 3)))

  ;;Internal form: catch exception and return.  External form: catch exception and escape.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (list (condition? E)
				(serious-condition? E)
				(violation? E)
				(non-continuable-violation? E)
				(condition-who E)
				(condition-message E)
				(let ((C (car (condition-irritants E))))
				  (vector (condition? C)
					  (serious-condition? C)
					  (error? C)
					  (condition-who C)
					  (condition-message C)
					  (condition-irritants C))))))
	      (lambda ()
		(with-exception-handler
		    (lambda (E)
		      123)
		  (lambda ()
		    (raise (condition (make-error)
				      (make-who-condition 'me)
				      (make-message-condition "the message")
				      (make-irritants-condition '(1 2 3))))))))))
    => '(#t #t #t #t raise "handler returned from non-continuable exception"
	    #(#t #t #t me "the message" (1 2 3))))

;;; --------------------------------------------------------------------

  ;;Nested uses: raising a non-condition object to show the sequence of handlers.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (A)
		  (escape (cons 'A A)))
	      (lambda ()
		(with-exception-handler
		    (lambda (B)
		      (raise (cons 'B B)))
		  (lambda ()
		    (with-exception-handler
			(lambda (C)
			  (raise (cons 'C C)))
		      (lambda ()
			(raise 123)))))))))
    => '(A B C . 123))

  ;;Nested uses.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (A)
		  (escape (list (condition? A)
				(serious-condition? A)
				(error? A)
				(condition-who A)
				(condition-message A)
				(condition-irritants A))))
	      (lambda ()
		(with-exception-handler
		    (lambda (B)
		      (raise B))
		  (lambda ()
		    (with-exception-handler
			(lambda (C)
			  (raise C))
		      (lambda ()
			(raise
			 (condition (make-error)
				    (make-who-condition 'me)
				    (make-message-condition "the message")
				    (make-irritants-condition '(1 2 3))))))))))))
    => '(#t #t #t me "the message" (1 2 3)))

  (values))


(parameterise ((check-test-name		'raise))

  ;;Returning from a handler.
  ;;
  (check
      (let ((C (call/cc
		   (lambda (escape)
		     (with-exception-handler
			 escape
		       (lambda ()
			 (with-exception-handler
			     (lambda (obj) obj)
			   (lambda ()
			     (raise 123)))))))))
	(values (condition? C)
		(serious-condition? C)
		(violation? C)
		(non-continuable-violation? C)
		(condition-message C)
		(condition-irritants C)))
    => #t #t #t #t "handler returned from non-continuable exception" '(123))

  (values))


(parameterise ((check-test-name		'raise-continuable))

  ;;Returning from a handler.
  ;;
  (check
      (with-exception-handler
	  ;;This handler returns!
	  (lambda (obj)
	    (values 'obj obj))
	(lambda ()
	  (raise-continuable 123)))
    => 'obj 123)

  ;;Nested handlers.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (A)
		  (escape (cons 'A A)))
	      (lambda ()
		(with-exception-handler
		    (lambda (B)
		      (raise (cons 'B B)))
		  (lambda ()
		    (with-exception-handler
			(lambda (C)
			  (raise (cons 'C C)))
		      (lambda ()
			(raise-continuable 123)))))))))
    => '(A B C . 123))

  (values))


(parameterise ((check-test-name		'warnings))

  ;;When a "&warning"  is raised and uncaught:  the default exception handlers will  print a message
  ;;to the CURRENT-ERROR-PORT and continue.
  ;;
  (check
      (begin
	(raise-continuable
	 (condition (make-warning)
		    (make-who-condition 'me)
		    (make-message-condition "the message")
		    (make-irritants-condition '(1 2 3))))
	123)
    => 123)

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file

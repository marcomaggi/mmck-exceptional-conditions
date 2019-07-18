;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for RETURNABLE
;;;Date: Jul 18, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the syntax RETURNABLE.
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

(module (test-returnable)
    ()
  (import (scheme)
    (only (chicken base)
	  make-parameter
	  parameterize)
    (mmck exceptional-conditions)
    (mmck exceptional-conditions helpers)
    (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing RETURNABLE\n")


;;;; helpers

(define-syntax define-returnable
  (syntax-rules ()
    ((_ (?name . ?formals) ?body0 ?body ...)
     (define (?name . ?formals)
       (call/cc
	   (lambda (escape)
	     (parameterize ((mmck-return-handler escape))
	       ?body0 ?body ...)))))
    ))

(define-syntax lambda-returnable
  (syntax-rules ()
    ((_ ?formals ?body0 ?body ...)
     (lambda ?formals
       (call/cc
	   (lambda (escape)
	     (parameterize ((mmck-return-handler escape))
	       ?body0 ?body ...)))))
    ))

(define-syntax begin-returnable
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (call/cc
	 (lambda (escape)
	   (parameterize ((mmck-return-handler escape))
	     ?body0 ?body ...))))
    ))


(parameterise ((check-test-name		'returnable))

  (check	;no return, no arguments
      (with-result
	(let ()
	  (define (ciao)
	    (returnable
	      (add-result 'in)
	      (add-result 'out)
	      1))
	  (ciao)))
    => '(1 (in out)))

  (check	;no return, arguments
      (with-result
	(let ()
	  (define (ciao a b)
	    (returnable
	      (add-result 'in)
	      (add-result 'out)
	      (list a b)))
	  (ciao 1 2)))
    => '((1 2) (in out)))

  (check	;return no values
      (with-result
	(let ()
	  (define (ciao)
	    (returnable
	      (add-result 'in)
	      (return)
	      (add-result 'out)
	      1))
	  (call-with-values
	      (lambda ()
		(ciao))
	    list)))
    => '(() (in)))

  (check	;return single value
      (with-result
	(let ()
	  (define (ciao)
	    (returnable
	      (add-result 'in)
	      (return 2)
	      (add-result 'out)
	      1))
	  (ciao)))
    => '(2 (in)))

  (check	;return multiple values
      (with-result
	(let ()
	  (define (ciao)
	    (returnable
	      (add-result 'in)
	      (return 2 3 4)
	      (add-result 'out)
	      (values 1 2 3)))
	  (receive (a b c)
	      (ciao)
	    (list a b c))))
    => '((2 3 4) (in)))

  (values))


(parameterise ((check-test-name		'define-returnable))

  (check	;no return, no arguments
      (with-result
	(let ()
	  (define-returnable (ciao)
	    (add-result 'in)
	    (add-result 'out)
	    1)
	  (ciao)))
    => '(1 (in out)))

  (check	;no return, arguments
      (with-result
	(let ()
	  (define-returnable (ciao a b)
	    (add-result 'in)
	    (add-result 'out)
	    (list a b))
	  (ciao 1 2)))
    => '((1 2) (in out)))

  (check	;return no values
      (with-result
	(let ()
	  (define-returnable (ciao)
	    (add-result 'in)
	    (return)
	    (add-result 'out)
	    1)
	  (call-with-values
	      (lambda ()
		(ciao))
	    list)))
    => '(() (in)))

  (check	;return single value
      (with-result
	(let ()
	  (define-returnable (ciao)
	    (add-result 'in)
	    (return 2)
	    (add-result 'out)
	    1)
	  (ciao)))
    => '(2 (in)))

  (check	;return multiple values
      (with-result
	(let ()
	  (define-returnable (ciao)
	    (add-result 'in)
	    (return 2 3 4)
	    (add-result 'out)
	    (values 1 2 3))
	  (receive (a b c)
	      (ciao)
	    (list a b c))))
    => '((2 3 4) (in)))

  (values))


(parameterise ((check-test-name		'lambda-returnable))

  (check	;no return, no arguments
      (with-result
	(let ()
	  (define ciao
	    (lambda-returnable ()
	      (add-result 'in)
	      (add-result 'out)
	      1))
	  (ciao)))
    => '(1 (in out)))

  (check	;no return, arguments
      (with-result
	(let ()
	  (define ciao
	    (lambda-returnable (a b)
	      (add-result 'in)
	      (add-result 'out)
	      (list a b)))
	  (ciao 1 2)))
    => '((1 2) (in out)))

  (check	;return no values
      (with-result
	(let ()
	  (define ciao
	    (lambda-returnable ()
	      (add-result 'in)
	      (return)
	      (add-result 'out)
	      1))
	  (call-with-values
	      (lambda () (ciao))
	    list)))
    => '(() (in)))

  (check	;return single value
      (with-result
	(let ()
	  (define ciao
	    (lambda-returnable ()
	      (add-result 'in)
	      (return 2)
	      (add-result 'out)
	      1))
	  (ciao)))
    => '(2 (in)))

  (check	;return multiple values
      (with-result
	(let ()
	  (define ciao
	    (lambda-returnable ()
	      (add-result 'in)
	      (return 2 3 4)
	      (add-result 'out)
	      (values 1 2 3)))
	  (receive (a b c)
	      (ciao)
	    (list a b c))))
    => '((2 3 4) (in)))

  (values))


(parameterise ((check-test-name		'begin-returnable))

  (check	;no return, no arguments
      (with-result
	(begin-returnable
	  (add-result 'in)
	  (add-result 'out)
	  1))
    => '(1 (in out)))

  (check	;no return, arguments
      (with-result
	(begin-returnable
	  (add-result 'in)
	  (add-result 'out)
	  (list 1 2)))
    => '((1 2) (in out)))

  (check	;return no values
      (with-result
	(call-with-values
	    (lambda ()
	      (begin-returnable
		(add-result 'in)
		(return)
		(add-result 'out)
		1))
	  list))
    => '(() (in)))

  (check	;return single value
      (with-result
	(begin-returnable
	  (add-result 'in)
	  (return 2)
	  (add-result 'out)
	  1))
    => '(2 (in)))

  (check	;return multiple values
      (with-result
	(receive (a b c)
	    (begin-returnable
	      (add-result 'in)
	      (return 2 3 4)
	      (add-result 'out)
	      (values 1 2 3))
	  (list a b c)))
    => '((2 3 4) (in)))

  (values))


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file

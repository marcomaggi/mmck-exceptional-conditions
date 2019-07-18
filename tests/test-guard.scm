;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for GUARD
;;;Date: Jul 18, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the syntax GUARD.
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

(module (test-guard)
    ()
  (import (scheme)
    (mmck exceptional-conditions)
    (mmck exceptional-conditions helpers)
    (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing GUARD\n")


;;;; helpers



(parameterise ((check-test-name		'body-return-values))

  ;;No exception.  Return single value.
  ;;
  (check
      (guard (E ((error? E)
		 'error)
		((warning? E)
		 'warning)
		(else 123))
	1)
    => 1)

  ;;No exception.  Return single value.  No ELSE clause.
  ;;
  (check
      (guard (E ((error? E)
		 'error)
		((warning? E)
		 'warning))
	1)
    => 1)

  ;;No exception.  Return zero values.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E ((error? E)
		       'error)
		      ((warning? E)
		       'warning)
		      (else 123))
	      (values)))
	(lambda args args))
    => '())

  ;;No exception.  Return 3 values.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E ((error? E)
		       'error)
		      ((warning? E)
		       'warning)
		      (else 123))
	      (values 1 2 3)))
	(lambda args args))
    => '(1 2 3))

  (values))


(parameterise ((check-test-name		'exception-with-else-clause))

  ;;Raise an exception.  Only ELSE clause.
  ;;
  (check
      (guard (E (else 456))
	(raise 123))
    => 456)

  ;;Raise an exception.  No clauses matches.
  ;;
  (check
      (guard (E ((error? E)
		 'error)
		((warning? E)
		 'warning)
		(else 456))
	(raise 123))
    => 456)

  ;;ELSE clause returns zero values.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E (else (values)))
	      (raise 123)))
	list)
    => '())

  ;;ELSE clause returns one value.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E (else (values 1)))
	      (raise 123)))
	list)
    => '(1))

  ;;ELSE clause returns three values.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E (else (values 1 2 3)))
	      (raise 123)))
	list)
    => '(1 2 3))

;;; --------------------------------------------------------------------

  ;;Raise an exception.  First clause matches.
  ;;
  (check
      (guard (E ((error? E)
		 'error)
		((warning? E)
		 'warning)
		(else 456))
	(raise (make-error)))
    => 'error)

  ;;Raise an exception.  Second clause matches.
  ;;
  (check
      (guard (E ((error? E)
		 'error)
		((warning? E)
		 'warning)
		(else 456))
	(raise (make-warning)))
    => 'warning)

;;; --------------------------------------------------------------------

  ;;Clause returns zero values.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E ((error? E)
		       (values))
		      (else
		       (values)))
	      (raise (make-error))))
	list)
    => '())

  ;;ELSE clause returns one value.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E ((error? E)
		       1)
		      (else (values 1)))
	      (raise (make-error))))
	list)
    => '(1))

  ;;ELSE clause returns three values.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E ((error? E)
		       (values 1 2 3))
		      (else
		       (values 1 2 3)))
	      (raise (make-error))))
	list)
    => '(1 2 3))

  (values))


(parameterise ((check-test-name		'exception-without-else-clause))

  ;;Raise an exception.  No clauses matches.  The exception is raised again using RAISE-CONTINUABLE.
  ;;Do not continue.
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (obj)
		  (escape obj))
	      (lambda ()
		(guard (E ((error? E)
			   'error)
			  ((warning? E)
			   'warning))
		  (raise 123))))))
    => 123)

  ;;Raise an  exception with  RAISE-CONTINUE.  No  clauses matches.  The  exception is  raised again
  ;;using RAISE-CONTINUABLE.  Do continue.
  ;;
  (check
      (with-exception-handler
	  (lambda (obj)
	    (list obj 456))
	(lambda ()
	  (guard (E ((error? E)
		     'error)
		    ((warning? E)
		     'warning))
	    (raise-continuable 123))))
    => '(123 456))

  ;;Raise  an exception  with RAISE.   No  clauses matches.   The  exception is  raised again  using
  ;;RAISE-CONTINUABLE.   Do continue.   After  continuing:  another exception  is  raised with  kind
  ;;"&non-continuable".
  ;;
  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (list (non-continuable-violation? E)
				(condition-who E)
				(condition-message E)
				(condition-irritants (car (condition-irritants E))))))
	      (lambda ()
		(with-exception-handler
		    ;;This handler contiues!
		    (lambda (obj)
		      (list obj 456))
		  (lambda ()
		    (guard (E ((error? E)
			       'error)
			      ((warning? E)
			       'warning))
		      (raise 123))))))))
    => '(#t raise "handler returned from non-continuable exception" (123)))

;;; -------------------------------------------------------------------- ;

  ;;Raise an exception.  First clause matches.
  ;;
  (check
      (guard (E ((error? E)
		 'error)
		((warning? E)
		 'warning))
	(raise (make-error)))
    => 'error)

  ;;Raise an exception.  Second clause matches.
  ;;
  (check
      (guard (E ((error? E)
		 'error)
		((warning? E)
		 'warning))
	(raise (make-warning)))
    => 'warning)

;;; -------------------------------------------------------------------- ;

  ;;Clause returns zero values.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E ((error? E)
		       (values)))
	      (raise (make-error))))
	list)
    => '())

  ;;ELSE clause returns one value.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E ((error? E)
		       1))
	      (raise (make-error))))
	list)
    => '(1))

  ;;ELSE clause returns three values.
  ;;
  (check
      (call-with-values
	  (lambda ()
	    (guard (E ((error? E)
		       (values 1 2 3)))
	      (raise (make-error))))
	list)
    => '(1 2 3))

  (values))


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file

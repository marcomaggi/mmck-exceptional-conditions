;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for miscellaneous macros
;;;Date: Jul 19, 2019
;;;
;;;Abstract
;;;
;;;	This program tests miscellaneous macros.
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

(module (test-misc-macros)
    ()
  (import (scheme)
    (mmck exceptional-conditions)
    (mmck exceptional-conditions helpers)
    (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing miscellaneous macros\n")


;;;; helpers



(parameterise ((check-test-name		'with-blocked-exceptions))

  ;;No exception.  Return single value.
  ;;
  (check
      (with-blocked-exceptions
	  (lambda ()
	    1))
    => 1)

  (check
      (with-blocked-exceptions
	  (lambda ()
	    (values 1 2 3)))
    => 1 2 3)

  (check
      (call-with-values
	  (lambda ()
	    (with-blocked-exceptions
		(lambda ()
		  (values))))
	list)
    => '())

;;; --------------------------------------------------------------------

  (check
      (with-blocked-exceptions
	  (lambda ()
	    (raise 123)))
    => 123)

  (check
      (with-blocked-exceptions
	  (lambda (E)
	    (values E 1 2 3))
	(lambda ()
	  (raise 99)))
    => 99 1 2 3)

  (values))


(parameterise ((check-test-name		'with-current-dynamic-environment))

  (define parm
    (make-parameter #f))

  (check
      (with-result
	(parameterise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parameterise ((parm 'inner))
			    (with-current-dynamic-environment
				values
			      (lambda ()
				(set! counter (+ 1 counter))
				(add-result (list 'inside-thunk (parm))))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner)
	    calling-thunk-2 (inside-thunk inner))))

  (check	;raising exception
      (with-result
	(parameterise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parameterise ((parm 'inner))
			    (with-current-dynamic-environment
				values
			      (lambda ()
				(set! counter (+ 1 counter))
				(add-result (list 'inside-thunk (parm)))
				(add-result 'raise-exception)
				(raise 123))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (let ((A (thunk)))
	      (add-result 'calling-thunk-2)
	      (let ((B (thunk)))
		(values A B counter))))))
    => '(123 123 2
	     (outer
	      calling-thunk-1 (inside-thunk inner) raise-exception
	      calling-thunk-2 (inside-thunk inner) raise-exception)))


  (values))


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file

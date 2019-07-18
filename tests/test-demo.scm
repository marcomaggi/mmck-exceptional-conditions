;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for casual tests
;;;Date: Jul 18, 2019
;;;
;;;Abstract
;;;
;;;	This program tests casual stuff.
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

(module (test-demo)
    ()
  (import (scheme)
    (mmck exceptional-conditions)
    (mmck exceptional-conditions helpers)
    (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing demo\n")


;;;; helpers



(parameterise ((check-test-name		'stuff))

  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape (list (non-continuable-violation? E)
				(condition-who E)
				(condition-message E))))
	      (lambda ()
		(with-exception-handler
		    ;;This handler contiues!
		    (lambda (obj)
		      #;(debug-print 'test 'returning-from-internal-handler obj)
		      (list obj 456))
		  (lambda ()
		    (guard (E ((error? E)
			       'error)
			      ((warning? E)
			       'warning))
		      (raise 123))))))))
    => '(#t raise "handler returned from non-continuable exception"))

  (values))


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file

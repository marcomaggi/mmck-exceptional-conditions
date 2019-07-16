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
	  (mmck exceptional-conditions)
	  (mmck exceptional-conditions helpers)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing exception handlers\n")


(parameterise ((check-test-name		'with-exception-handler))

  (check
      (call-with-current-continuation
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape 456))
	      (lambda ()
		123))))
    => 123)

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

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file

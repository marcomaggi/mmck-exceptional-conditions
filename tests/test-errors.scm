;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for error functions
;;;Date: Jul 16, 2019
;;;
;;;Abstract
;;;
;;;	This program tests error functions.
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

(module (test-error)
    ()
  (import (scheme)
	  (mmck exceptional-conditions)
	  (mmck exceptional-conditions helpers)
	  (mmck checks)
	  (only (chicken condition)
		condition-case))

(check-set-mode! 'report-failed)
(check-display "*** testing error functions\n")


(parameterise ((check-test-name		'errors))

  (check
      (condition-case
	  (error 'me "the message" 1 2 3)
	(E (&error)
	   (list (condition? E)
		 (serious-condition? E)
		 (error? E)
		 (condition-who E)
		 (condition-message E)
		 (condition-irritants E)))
	(E ()
	   E))
    => '(#t #t #t me "the message" (1 2 3)))

  (values))


(parameterise ((check-test-name		'assertions))

  (check
      (condition-case
	  (assertion-violation 'me "the message" 1 2 3)
	(E (&assertion)
	   (list (condition? E)
		 (serious-condition? E)
		 (assertion-violation? E)
		 (condition-who E)
		 (condition-message E)
		 (condition-irritants E)))
	(E ()
	   E))
    => '(#t #t #t me "the message" (1 2 3)))

;;; --------------------------------------------------------------------

  (check
      (condition-case
	  (let ()
	    (define (doit)
	      123)
	    (assert (doit)))
	(E (&assertion)
	   (list (condition? E)
		 (serious-condition? E)
		 (assertion-violation? E)
		 (condition-who E)
		 (condition-message E)
		 (condition-irritants E)))
	(E ()
	   E))
    => 123)

  (check
      (condition-case
	  (let ()
	    (define (doit)
	      #f)
	    (assert (doit)))
	(E (&assertion)
	   (list (condition? E)
		 (serious-condition? E)
		 (assertion-violation? E)
		 (condition-who E)
		 (condition-message E)
		 (condition-irritants E)))
	(E ()
	   E))
    => '(#t #t #t assert "failed assertion" ((doit))))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file

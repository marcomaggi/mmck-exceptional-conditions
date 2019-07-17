;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for CHICKEN compatibility
;;;Date: Jul 16, 2019
;;;
;;;Abstract
;;;
;;;	This program tests CHICKEN compatibility.
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

(module (test-chicken-compat)
    ()
  (import (scheme)
	  (only (chicken base)
		call/cc
		error
		exit)
	  (prefix (only (chicken base)
			error)
		  chicken::)
	  (prefix (chicken condition)
		  chicken::)
	  (mmck exceptional-conditions)
	  (mmck exceptional-conditions helpers)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing CHICKEN compatibility\n")

(current-exception-handler predefined-exception-handler)


(parameterise ((check-test-name		'chicken::with-exception-handler))

  ;;Test escaping, which would be required with CHICKEN's default exception handler.
  ;;
  (check
      (call-with-current-continuation
	  (lambda (escape)
	    (chicken::with-exception-handler
		(lambda (E)
		  (escape 456))
	      (lambda ()
		123))))
    => 123)

  ;;Raising an exception with CHICKEN::ERROR.
  ;;
  (check
      (call-with-current-continuation
	  (lambda (escape)
	    (chicken::with-exception-handler
		(lambda (E)
		  (escape (list (chicken::get-condition-property E 'exn 'location)
				(chicken::get-condition-property E 'exn 'message)
				(chicken::get-condition-property E 'exn 'arguments))))
	      (lambda ()
		(chicken::error 'me "the message" 1 2 3)))))
    => '(me "the message" (1 2 3)))

  ;;Raising an exception with ERROR.
  ;;
  (check
      (call-with-current-continuation
	  (lambda (escape)
	    (chicken::with-exception-handler
		(lambda (E)
		  (escape (list (condition-who E)
				(condition-message E)
				(condition-irritants E))))
	      (lambda ()
		(error 'me "the message" 1 2 3)))))
    => '(me "the message" (1 2 3)))

  (values))


#;(parameterise ((check-test-name		'chicken::condition-case))

;;; no vars

  (check
      (chicken::condition-case
	  (chicken::signal
	   (chicken::make-property-condition 'demo
	     'location 'me
	     'message "the message"
	     'arguments '(1 2 3)))
	(()
	 'here))
    => 'here)

  (check
      (chicken::condition-case
	  (chicken::signal
	   (chicken::make-property-condition '&error
	     'location 'me
	     'message "the message"
	     'arguments '(1 2 3)))
	((&warning)
	 'warning)
	((&error)
	 'error)
	(()
	 'else))
    => 'error)

  (check
      (chicken::condition-case
	  (chicken::signal
	   (chicken::make-property-condition 'demo
	     'location 'me
	     'message "the message"
	     'arguments '(1 2 3)))
	((&warning)
	 'warning)
	((&error)
	 'error)
	(()
	 'else))
    => 'else)

;;; --------------------------------------------------------------------
;;; no vars

  (check
      (chicken::condition-case
	  (chicken::signal
	   (chicken::make-composite-condition
	    (chicken::make-property-condition '&error
	      'location 'me
	      'message "the message"
	      'arguments '(1 2 3))
	    (chicken::make-property-condition '&serious)
	    (chicken::make-property-condition '&condition)))
	(var ()
	     (list 'here (condition-kinds var))))
    => '(here (&error &serious &condition)))

  (check
      (chicken::condition-case
	  (chicken::signal
	   (chicken::make-composite-condition
	    (chicken::make-property-condition '&error
	      'location 'me
	      'message "the message"
	      'arguments '(1 2 3))
	    (chicken::make-property-condition '&serious)
	    (chicken::make-property-condition '&condition)))
	((&warning)
	 'warning)
	(var (&error)
	     (list 'error (condition-kinds var)))
	(()
	 'else))
    => '(error (&error &serious &condition)))

  (check
      (chicken::condition-case
	  (chicken::signal
	   (chicken::make-composite-condition
	    (chicken::make-property-condition '&warning
	      'location 'me
	      'message "the message"
	      'arguments '(1 2 3))
	    (chicken::make-property-condition '&condition)))

	((&syntax)
	 'syntax)
	((&error)
	 'error)
	(var ()
	     (list 'else (condition-kinds var))))
    => '(else (&warning &condition)))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file

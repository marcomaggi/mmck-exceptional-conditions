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


#;(parameterise ((check-test-name		'condition-case-equiv))

  (define-syntax condition-case-clause
    (syntax-rules ()
      ((_ ?escape ?kindvar ?cndvar)
       (raise ?cndvar))

      ((_ ?escape ?kindvar ?cndvar (() ?body0 ?body ...) ?clause ...)
       (?escape (begin ?body0 ?body ...)))

      ((_ ?escape ?kindvar ?cndvar (() ?body0 ?body ...) ?clause ...)
       (?escape (let ((?cndvar1 ?cndvar))
		  ?body0 ?body ...)))

      ((_ ?escape ?kindvar ?cndvar (?kind ?body0 ?body ...) ?clause ...)
       (if (memv ?kind ?kindvar)
	   (?escape (begin ?body0 ?body ...))
	 (condition-case-clause ?escape ?kindvar ?cndvar ?clause ...)))

      ((_ ?escape ?kindvar ?cndvar (?cndvar1 ?kind ?body0 ?body ...) ?clause ...)
       (if (memv ?kind ?kindvar)
	   (?escape (let ((?cndvar1 ?cndvar))
		      ?body0 ?body ...))
	 (condition-case-clause ?escape ?kindvar ?cndvar ?clause ...)))
      ))

  (define-syntax condition-case-equiv
    (syntax-rules ()
      ((_ ?expr ?clause0 ?clause ...)
       (call/cc
	   (lambda (escape)
	     (with-exception-handler
		 (lambda (cndvar)
		   (let ((kinds (condition-kinds cndvar)))
		     (condition-case-clause escape kinds cndvar ?clause0 ?clause ...)))
	       (lambda ()
		 ?expr)))))
      ))

;;; --------------------------------------------------------------------
;;; no vars

  (check
      (condition-case-equiv
	  (raise (make-error))
	(()
	 'here))
    => 'here)

  (check
      (condition-case-equiv
	  (raise (make-error))
	((&undefined)
	 'undefined)
	((&error)
	 'error)
	(()
	 'else))
    => 'error)

  (check
      (condition-case-equiv
	  (raise (make-who-condition 'me))
	((&undefined)
	 'undefined)
	((&error)
	 'error)
	(()
	 'else))
    => 'else)

  (values))


(parameterise ((check-test-name		'condition-case))

;;; no vars

  (check
      (condition-case
	  (raise (make-error))
	(()
	 'here))
    => 'here)

  (check
      (condition-case
	  (raise (make-error))
	((&warning)
	 'warning)
	((&error)
	 'error)
	(()
	 'else))
    => 'error)

  (check
      (condition-case
	  (raise (make-who-condition 'me))
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
      (condition-case
	  (raise (make-error))
	(var ()
	     (list 'here (condition-kinds var))))
    => '(here (&error &serious &condition)))

  (check
      (condition-case
	  (raise (make-error))
	((&warning)
	 'warning)
	(var (&error)
	     (list 'error (condition-kinds var)))
	(()
	 'else))
    => '(error (&error &serious &condition)))

  (check
      (condition-case
	  (raise (make-warning))
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

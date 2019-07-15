;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for exceptional-condition objects
;;;Date: Jul 13, 2019
;;;
;;;Abstract
;;;
;;;	This program tests exceptional-condition objects.
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

(require-library (mmck exceptional-conditions)
		 (mmck checks))

(module (test-demo)
    ()
  (import (scheme)
	  (chicken base)
	  (mmck exceptional-conditions)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing exceptional condition objects\n")


;;;; helpers

(define (debug-print . args)
  (import (chicken base)
	  (chicken pretty-print))
  (parameterize ((pretty-print-width 150))
    (pretty-print args (current-error-port))))


(parameterize ((check-test-name		'&condition))

  (check
      (let ((C	(make-condition)))
	(values (condition?         C)
		(message-condition? C)))
    => #t #f)

  (check
      (let ((C	(condition (make-condition)
			   (make-condition))))
	(values (condition?         C)
		(message-condition? C)))
    => #t #f)

  #f)


(parameterize ((check-test-name		'&who))

  (check
      (let ((C	(make-who-condition 'ciao)))
	(values (condition?         C)
		(who-condition?     C)
		(message-condition? C)
		(condition-who      C)))
    => #t #t #f 'ciao)

  #f)


(parameterize ((check-test-name		'&message))

  (check
      (let ((C	(make-message-condition "hello world")))
	(values (condition?         C)
		(message-condition? C)
		(who-condition?     C)
		(condition-message  C)))
    => #t #t #f "hello world")

  #f)


(parameterize ((check-test-name		'&irritants))

  (check
      (let ((C	(make-irritants-condition '(1 2 3))))
	(values (condition?         C)
		(irritants-condition? C)
		(who-condition?     C)
		(condition-irritants  C)))
    => #t #t #f '(1 2 3))

  #f)


(parameterize ((check-test-name		'&warning))

  (check
      (let ((C	(make-warning)))
	(values (condition?         C)
		(warning?           C)
		(message-condition? C)))
    => #t #t #f)

  #f)


(parameterize ((check-test-name		'&serious))

  (check
      (let ((C	(make-serious-condition)))
	(values (condition?         C)
		(serious-condition? C)
		(message-condition? C)))
    => #t #t #f)

  #f)


(parameterize ((check-test-name		'&error))

  (check
      (let ((C	(make-error)))
	(values (condition?         C)
		(serious-condition? C)
		(error?             C)
		(message-condition? C)))
    => #t #t #t #f)

  #f)


(parameterize ((check-test-name		'&violation))

  (check
      (let ((C	(make-violation)))
	(values (condition?         C)
		(serious-condition? C)
		(violation?         C)
		(message-condition? C)))
    => #t #t #t #f)

  #f)


(parameterize ((check-test-name		'&assertion))

  (check
      (let ((C	(make-assertion-violation)))
	(values (condition?		C)
		(serious-condition?	C)
		(assertion-violation?	C)
		(message-condition?	C)))
    => #t #t #t #f)

  #f)


(parameterize ((check-test-name		'&non-continuable))

  (check
      (let ((C	(make-non-continuable-violation)))
	(values (condition?			C)
		(serious-condition?		C)
		(non-continuable-violation?	C)
		(message-condition?		C)))
    => #t #t #t #f)

  #f)


(parameterize ((check-test-name		'&implementation-restriction))

  (check
      (let ((C	(make-implementation-restriction-violation)))
	(values (condition?		C)
		(serious-condition?	C)
		(implementation-restriction-violation?	C)
		(message-condition?	C)))
    => #t #t #t #f)

  #f)


(parameterize ((check-test-name		'&lexical))

  (check
      (let ((C	(make-lexical-violation)))
	(values (condition?		C)
		(serious-condition?	C)
		(lexical-violation?	C)
		(message-condition?	C)))
    => #t #t #t #f)

  #f)


(parameterize ((check-test-name		'&syntax))

  (check
      (let ((C	(make-syntax-violation)))
	(values (condition?		C)
		(serious-condition?	C)
		(syntax-violation?	C)
		(message-condition?	C)))
    => #t #t #t #f)

  #f)


(parameterize ((check-test-name		'&undefined))

  (check
      (let ((C	(make-undefined-violation)))
	(values (condition?		C)
		(serious-condition?	C)
		(undefined-violation?	C)
		(message-condition?	C)))
    => #t #t #t #f)

  #f)


(parameterize ((check-test-name		'compound))

  (check
      (let ((C	(condition (make-who-condition 'ciao)
			   (make-message-condition "hello world")
			   (make-irritants-condition '(1 2 3)))))
	(values (condition?		C)
		(serious-condition?	C)
		(who-condition?		C)
		(message-condition?	C)
		(irritants-condition?	C)))
    => #t #f #t #t #t)

  (check
      (let ((C	(condition (make-who-condition 'ciao)
			   (condition (make-message-condition "hello world")
				      (make-irritants-condition '(1 2 3))))))
	(values (condition?		C)
		(serious-condition?	C)
		(who-condition?		C)
		(message-condition?	C)
		(irritants-condition?	C)))
    => #t #f #t #t #t)

  (check
      (let ((C	(condition (condition (make-who-condition 'ciao)
				      (make-message-condition "hello world")
				      (make-irritants-condition '(1 2 3))))))
	(values (condition?		C)
		(serious-condition?	C)
		(who-condition?		C)
		(message-condition?	C)
		(irritants-condition?	C)))
    => #t #f #t #t #t)

  (check
      (let ((C	(condition)))
	(values (condition?		C)
		(serious-condition?	C)
		(who-condition?		C)
		(message-condition?	C)
		(irritants-condition?	C)))
    => #t #f #f #f #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((C	(condition))
	     (S	(simple-conditions C)))
	(values (length S)
		(condition? (car S))
		(serious-condition? (car S))))
    => 1 #t #f)

  (check
      (let* ((C	(condition (make-who-condition 'ciao)
			   (make-message-condition "hello world")
			   (make-irritants-condition '(1 2 3))))
	     (S	(simple-conditions C)))
	#;(debug-print S)
	(values (length			S)
		(condition?		C)
		(who-condition?		(list-ref S 0))
		(message-condition?	(list-ref S 2))
		(irritants-condition?	(list-ref S 4))))
    => 6 #t #t #t #t)

  (check
      (let* ((C	(condition (make-who-condition 'ciao)
			   (condition (make-message-condition "hello world")
				      (make-irritants-condition '(1 2 3)))))
	     (S	(simple-conditions C)))
	(values (length			S)
		(condition?		C)
		(who-condition?		(list-ref S 0))
		(message-condition?	(list-ref S 2))
		(irritants-condition?	(list-ref S 4))))
    => 6 #t #t #t #t)

  (check
      (let* ((C	(condition (condition (make-who-condition 'ciao)
				      (make-message-condition "hello world"))
			   (make-irritants-condition '(1 2 3))))
	     (S	(simple-conditions C)))
	(values (length			S)
		(condition?		C)
		(who-condition?		(list-ref S 0))
		(message-condition?	(list-ref S 2))
		(irritants-condition?	(list-ref S 4))))
    => 6 #t #t #t #t)

  #f)


(parameterize ((check-test-name		'docs))

  (let ()
    (define-condition-type &c
	&condition
      make-c c?
      (x c-x))

    (define-condition-type &c1
	&c
      make-c1 c1?
      (a c1-a))

    (define-condition-type &c2
	&c
      make-c2 c2?
      (b c2-b))

    (define v1 (make-c1 "V1" "a1"))

    (check (c? v1)        => #t)
    (check (c1? v1)       => #t)
    (check (c2? v1)       => #f)
    (check (c-x v1)       => "V1")
    (check (c1-a v1)      => "a1")



    (define v2 (make-c2 "V2" "b2"))

    (check (c? v2)        => #t)
    (check (c1? v2)       => #f)
    (check (c2? v2)       => #t)
    (check (c-x v2)       => "V2")
    (check (c2-b v2)      => "b2")



    (define v3 (condition
		 (make-c1 "V3/1" "a3")
		 (make-c2 "V3/2" "b3")))

    (check (c? v3)        => #t)
    (check (c1? v3)       => #t)
    (check (c2? v3)       => #t)
    (check (c-x v3)       => "V3/1")
    (check (c1-a v3)      => "a3")
    (check (c2-b v3)      => "b3")



    (define v4 (condition v1 v2))

    (check (c? v4)        => #t)
    (check (c1? v4)       => #t)
    (check (c2? v4)       => #t)
    (check (c-x v4)       => "V1")
    (check (c1-a v4)      => "a1")
    (check (c2-b v4)      => "b2")



    (define v5 (condition v2 v3))

    (check (c? v5)        => #t)
    (check (c1? v5)       => #t)
    (check (c2? v5)       => #t)
    (check (c-x v5)       => "V2")
    (check (c1-a v5)      => "a3")
    (check (c2-b v5)      => "b2")

    #f)

  #f)


(parameterize ((check-test-name		'fields))

  ;;Test splitting arguments for constructors.
  (let ()
    (define-condition-type &alpha
	&condition
      make-alpha
      alpha?
      (a	alpha-a))

    (define-condition-type &beta
	&alpha
      make-beta
      beta?
      (b1	beta-b1)
      (b2	beta-b2))

    (define-condition-type &delta
	&beta
      make-delta
      delta?
      (d1	delta-d1)
      (d2	delta-d2)
      (d3	delta-d3))

    (define-condition-type &gamma
	&delta
      make-gamma
      gamma?
      (g1	gamma-g1)
      (g2	gamma-g2)
      (g3	gamma-g3)
      (g4	gamma-g4))

    (define A
      (make-alpha 1))

    (define B
      (make-beta 1
		 2 3))

    (define D
      (make-delta 1
		  2 3
		  4 5 6))

    (define G
      (make-gamma 1
		  2 3
		  4 5 6
		  7 8 9 0))

    (check
	(alpha-a	A)
      => 1)

    (check
	(values (alpha-a	B)
		(beta-b1	B)
		(beta-b2	B))
      => 1 2 3)

    (check
	(values (alpha-a	D)
		(beta-b1	D)
		(beta-b2	D)
		(delta-d1	D)
		(delta-d2	D)
		(delta-d3	D))
      => 1 2 3 4 5 6)

    (check
	(values (alpha-a	G)
		(beta-b1	G)
		(beta-b2	G)
		(delta-d1	G)
		(delta-d2	G)
		(delta-d3	G)
		(gamma-g1	G)
		(gamma-g2	G)
		(gamma-g3	G)
		(gamma-g4	G))
      => 1 2 3 4 5 6 7 8 9 0)

    #f)

  #f)


;;;; done

(check-report)

#| end of module |# )

;;; end of file

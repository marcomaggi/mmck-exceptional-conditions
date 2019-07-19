;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for coroutines
;;;Date: Jul 19, 2019
;;;
;;;Abstract
;;;
;;;	This program tests the coroutines.
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

(module (test-coroutines)
    ()
  (import (scheme)
    (mmck exceptional-conditions)
    (mmck exceptional-conditions helpers)
    (mmck checks)
    (only (chicken base)
	  letrec*
	  void)
    (only (chicken format)
	  format)
    (only (chicken port)
	  call-with-output-string))

(check-set-mode! 'report-failed)
(check-display "*** testing coroutines\n")


;;;; helpers



(parameterise ((check-test-name	'basic))

  (check
      (with-result
       (coroutine (lambda ()
		    (add-result 1)
		    (void)))
       #t)
    => `(#t (1)))

  (check
      (with-result
       (coroutine (lambda ()
		    (add-result 1)
		    (void)))
       (finish-coroutines)
       #t)
    => `(#t (1)))

  (check
      (with-result
	(let ()
	  (define (one)
	    (add-result '(one 1))
	    (coroutine two)
	    (add-result '(one 2))
	    (coroutine three)
	    (add-result '(one 3)))
	  (define (two)
	    (add-result '(two 1))
	    (yield)
	    (add-result '(two 2))
	    (yield)
	    (add-result '(two 3)))
	  (define (three)
	    (add-result '(three 1))
	    (yield)
	    (add-result '(three 2))
	    (yield)
	    (add-result '(three 3)))
	  (coroutine one)
	  (finish-coroutines)
	  #t))
    => `(#t
	 ((one 1) (two 1) (one 2) (three 1) (two 2) (one 3)
	  (three 2) (two 3) (three 3))))

  (check
      (with-result
       (letrec* ((one (lambda ()
			(add-result '(one 1))
			(yield)
			(add-result '(one 2))
			(yield)
			(add-result '(one 3))))
		 (two (lambda ()
			(add-result '(two 1))
			(yield)
			(add-result '(two 2))
			(yield)
			(add-result '(two 3)))))
	 (coroutine one)
	 (coroutine two)
	 (finish-coroutines))
       #t)
    => '(#t
	 ((one 1) (two 1)
	  (one 2) (two 2)
	  (one 3) (two 3))))

  #t)


(parameterise ((check-test-name	'examples))

;;; these are examples for the documentation

  (check	;two coroutines
      (call-with-output-string
	  (lambda (P)
	    (parameterise ((current-output-port P))
	      (coroutine
		  (lambda ()
		    (display "one 1\n")
		    (yield)
		    (display "one 2\n")
		    (yield)
		    (display "one 3\n")))
	      (coroutine
		  (lambda ()
		    (display "two 1\n")
		    (yield)
		    (display "two 2\n")
		    (yield)
		    (display "two 3\n")))
	      (finish-coroutines))))
    => "one 1\ntwo 1\none 2\ntwo 2\none 3\ntwo 3\n")

  (check	;main coroutine, one coroutine
      (call-with-output-string
	  (lambda (P)
	    (parameterise ((current-output-port P))
	      (coroutine
		  (lambda ()
		    (display "sub 1\n")
		    (yield)
		    (display "sub 2\n")
		    (yield)
		    (display "sub 3\n")))
	      (display "main 1\n")
	      (yield)
	      (display "main 2\n")
	      (yield)
	      (display "main 3\n")
	      (finish-coroutines))))
    => "sub 1\nmain 1\nsub 2\nmain 2\nsub 3\nmain 3\n")

  (check	;main coroutine alone
      (call-with-output-string
	  (lambda (P)
	    (parameterise ((current-output-port P))
	      (display "main 1\n")
	      (yield)
	      (display "main 2\n")
	      (yield)
	      (display "main 3\n")
	      (finish-coroutines))))
    => "main 1\nmain 2\nmain 3\n")

  (check	;main coroutine, one finishing coroutine, longer main
      (call-with-output-string
	  (lambda (P)
	    (parameterise ((current-output-port P))
	      (coroutine
		  (lambda ()
		    (display "sub 1\n")
		    (yield)
		    (display "sub 2\n")
		    (yield)
		    (display "sub 3\n")
		    (finish-coroutines)))
	      (display "main 1\n")
	      (yield)
	      (display "main 2\n")
	      (yield)
	      (display "main 3\n")
	      (yield)
	      (display "main 4\n")
	      (yield)
	      (display "main 5\n"))))
    => "sub 1\nmain 1\nsub 2\nmain 2\nsub 3\nmain 3\nmain 4\nmain 5\n")

  ;;Here there is still  an escape function in the queue, due to  the above test.  So
  ;;we reset it.
  #;(dump-coroutines)
  (reset-coroutines!)

  (check	;main coroutine, one finishing subroutine, longer subroutine
      (call-with-output-string
	  (lambda (P)
	    (parameterise ((current-output-port P))
	      (coroutine
		  (lambda ()
		    (display "sub 1\n")
		    (yield)
		    (display "sub 2\n")
		    (yield)
		    (display "sub 3\n")
		    (yield)
		    (display "sub 4\n")
		    (yield)
		    (display "sub 5\n")
		    (finish-coroutines)))
	      (display "main 1\n")
	      (yield)
	      (display "main 2\n")
	      (yield)
	      (display "main 3\n"))))
    => "sub 1\nmain 1\nsub 2\nmain 2\nsub 3\nmain 3\n")

  ;;Here there is still  an escape function in the queue, due to  the above test.  So
  ;;we reset it.
  #;(dump-coroutines)
  (reset-coroutines!)

  #t)


(parameterise ((check-test-name	'concurrently))

  (define (print template . args)
    (apply format (current-error-port) template args)
    (yield))

  (define (job N M)
    (print "sub ~a.~a\n" N M)
    (set! M (+ 1 M))
    (print "sub ~a.~a\n" N M)
    (set! M (+ 1 M))
    (print "sub ~a.~a\n" N M)
    (set! M (+ 1 M))
    (print "sub ~a.~a\n" N M)
    (set! M (+ 1 M))
    (print "sub ~a.~a\n" N M))

;;; --------------------------------------------------------------------
;;; no concurrently

  (check
      (let ((a #f) (b #f) (c #f))
	(coroutine
	    (lambda ()
	      (print "sub 1.1\n")
	      (print "sub 1.2\n")
	      (print "sub 1.3\n")
	      (set! a #t)))
	(coroutine
	    (lambda ()
	      (print "sub 2.1\n")
	      (print "sub 2.2\n")
	      (print "sub 2.3\n")
	      (set! b #t)))
	(coroutine
	    (lambda ()
	      (print "sub 3.1\n")
	      (print "sub 3.2\n")
	      (print "sub 3.3\n")
	      (set! c #t)))
	(finish-coroutines)
    	(values a b c))
    => #t #t #t)

;;; --------------------------------------------------------------------

  (check
      (let ((a #f) (b #f) (c #f))
	(concurrently
	  (lambda ()
	    (print "sub 1.1\n")
	    (print "sub 1.2\n")
	    (print "sub 1.3\n")
	    (set! a #t))
	  (lambda ()
	    (print "sub 2.1\n")
	    (print "sub 2.2\n")
	    (print "sub 2.3\n")
	    (set! b #t))
	  (lambda ()
	    (print "sub 3.1\n")
	    (print "sub 3.2\n")
	    (print "sub 3.3\n")
	    (set! c #t)))
	(values a b c))
    => #t #t #t)

;;; --------------------------------------------------------------------

  (check
      (let ((a #f) (b #f) (c #f))
	(concurrently
	  (lambda ()
	    (job 1 1)
	    (set! a #t))
	  (lambda ()
	    (job 2 1)
	    (set! b #t))
	  (lambda ()
	    (job 3 1)
	    (set! c #t)))
	(values a b c))
    => #t #t #t)

  #t)


(parameterise ((check-test-name	'monitor))

  (define (print template . args)
    (apply format (current-error-port) template args)
    (yield))

  (define (job N M)
    (print "monitor sub ~a.~a\n" N M)
    (set! M (+ 1 M))
    (print "monitor sub ~a.~a\n" N M)
    (set! M (+ 1 M))
    (print "monitor sub ~a.~a\n" N M)
    (set! M (+ 1 M))
    (print "monitor sub ~a.~a\n" N M)
    (set! M (+ 1 M))
    (print "monitor sub ~a.~a\n" N M))

  (define (monitor-job N M)
    (monitor 2
      (lambda ()
	(job N M))))

;;; --------------------------------------------------------------------

  (check
      (let ((a #f) (b #f) (c #f))
	(concurrently
	  (lambda ()
	    (monitor-job 1 1)
	    (set! a #t))
	  (lambda ()
	    (monitor-job 2 1)
	    (set! b #t))
	  (lambda ()
	    (monitor-job 3 1)
	    (set! c #t)))
	(values a b c))
    => #t #t #t)

  #t)


(parameterise ((check-test-name	'unwind-protect))

  (define (print template . args)
    (apply format (current-error-port) template args)
    (yield))

;;; --------------------------------------------------------------------

  (check
      (let ((a #f) (b #f) (c #f))
	(concurrently
	  (lambda ()
	    (unwind-protect
		(begin
		  (set! a 1.1)
		  (print "unwind-protect sub 1.1: ~a\n" a)
		  (set! a 1.2)
		  (print "unwind-protect sub 1.2: ~a\n" a)
		  (set! a 1.3)
		  (print "unwind-protect sub 1.3: ~a\n" a))
	      (set! a 1.4)))
	  (lambda ()
	    (unwind-protect
		(begin
		  (set! b 2.1)
		  (print "unwind-protect sub 2.1: ~a\n" b)
		  (set! b 2.2)
		  (print "unwind-protect sub 2.2: ~a\n" b)
		  (set! b 2.3)
		  (print "unwind-protect sub 2.3: ~a\n" b))
	      (set! b 2.4)))
	  (lambda ()
	    (unwind-protect
		(begin
		  (set! c 3.1)
		  (print "unwind-protect sub 3.1: ~a\n" c)
		  (set! c 3.2)
		  (print "unwind-protect sub 3.2: ~a\n" c)
		  (set! c 3.3)
		  (print "unwind-protect sub 3.3: ~a\n" c))
	      (set! c 3.4))))
	(values a b c))
    => 1.4 2.4 3.4)

  #t)


(parameterise ((check-test-name	'uid))

  (define (print template . args)
    (apply format (current-error-port) template args)
    (yield))

;;; --------------------------------------------------------------------

  (check
      (let ((a #f) (b #f) (c #f))
	(concurrently
	  (lambda ()
	    (unwind-protect
		(begin
		  (set! a 1.1)
		  (print "unwind-protect sub 1.1: ~a ~s\n" a (current-coroutine-uid))
		  (set! a 1.2)
		  (print "unwind-protect sub 1.2: ~a ~s\n" a (current-coroutine-uid))
		  (set! a 1.3)
		  (print "unwind-protect sub 1.3: ~a ~s\n" a (current-coroutine-uid)))
	      (set! a 1.4)))
	  (lambda ()
	    (unwind-protect
		(begin
		  (set! b 2.1)
		  (print "unwind-protect sub 2.1: ~a ~s\n" b (current-coroutine-uid))
		  (set! b 2.2)
		  (print "unwind-protect sub 2.2: ~a ~s\n" b (current-coroutine-uid))
		  (set! b 2.3)
		  (print "unwind-protect sub 2.3: ~a ~s\n" b (current-coroutine-uid)))
	      (set! b 2.4)))
	  (lambda ()
	    (unwind-protect
		(begin
		  (set! c 3.1)
		  (print "unwind-protect sub 3.1: ~a ~s\n" c (current-coroutine-uid))
		  (set! c 3.2)
		  (print "unwind-protect sub 3.2: ~a ~s\n" c (current-coroutine-uid))
		  (set! c 3.3)
		  (print "unwind-protect sub 3.3: ~a ~s\n" c (current-coroutine-uid)))
	      (set! c 3.4))))
	(values a b c))
    => 1.4 2.4 3.4)

  #t)


(parameterise ((check-test-name	'suspend-and-resume))

  (define (print template . args)
    (apply format (current-error-port) template args)
    (yield))

  (define-syntax-rule (log-action ?id ?action . ?args)
    (add-result (list (quote ?id) (quote ?action) . ?args)))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(define one)

	(coroutine
	    (lambda ()
	      (set! one (current-coroutine-uid))
	      (log-action one enter)
	      (yield)
	      (log-action one suspending)
	      (suspend-coroutine)
	      (log-action one resumed)
	      (log-action one done)))

	(coroutine
	    (lambda ()
	      (log-action two enter)
	      (yield)
	      (log-action two suspended? (suspended-coroutine? one))
	      (log-action two resuming-other)
	      (resume-coroutine one)
	      (log-action two done)))

	(finish-coroutines)
	#t)
    => '(#t ((one enter)
	     (two enter)
	     (one suspending)
	     (two suspended? #t)
	     (two resuming-other)
	     (one resumed)
	     (one done)
	     (two done))))

  #t)


;;;; done

(check-report)

#| end of MODULE |# )

;;; end of file

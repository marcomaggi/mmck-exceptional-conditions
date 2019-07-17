;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: exceptional-condition objects module
;;;Date: Jul 13, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the exceptional-condition objects types.
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

(declare (unit mmck.exceptional-conditions.condition-objects)
	 (uses mmck.exceptional-conditions.helpers)
	 (emit-import-library mmck.exceptional-conditions.condition-objects))

(module (mmck.exceptional-conditions.condition-objects)
    ((syntax: define-condition-type
	      chicken::make-composite-condition
	      chicken::make-property-condition
	      chicken::condition-predicate
	      chicken::condition-property-accessor
	      make-<condition>
	      <condition>-constructor
	      <condition>-total-number-of-fields
	      split-constructor-args
	      zip-constructor-args)
     ;; the following are defined by R6RS
     &condition condition?
     &who make-who-condition who-condition? condition-who
     &message make-message-condition message-condition? condition-message
     &irritants make-irritants-condition irritants-condition? condition-irritants
     &warning make-warning warning?
     &serious make-serious-condition serious-condition?
     &error make-error error?
     &violation &serious make-violation violation?
     &assertion &serious make-assertion-violation assertion-violation?
     &non-continuable make-non-continuable-violation non-continuable-violation?
     &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
     &lexical make-lexical-violation lexical-violation?
     &syntax make-syntax-violation syntax-violation?
     &undefined make-undefined-violation undefined-violation?
     ;;
     condition
     condition?
     simple-conditions
     condition-kinds
     ;;
     who-condition-value?
     message-condition-value?
     irritants-condition-value?)
  (import (scheme)
	  (prefix (only (chicken base)
			error
			define-record-printer)
		  chicken::)
	  (prefix (only (chicken condition)
			make-composite-condition
			make-property-condition
			condition?
			condition-property-accessor
			condition-predicate
			condition->list
			abort
			signal)
		  chicken::)
	  (only (chicken format)
		format)
	  (mmck exceptional-conditions helpers))
  (import-for-syntax (scheme)
		     (only (chicken base)
			   define-record)
		     (only (chicken syntax)
			   er-macro-transformer)
		     (only (matchable)
			   match)
		     (mmck exceptional-conditions helpers))


;;;; helpers

(define-syntax %internal-assert
  (syntax-rules ()
    ((_ ?expr)
     (unless ?expr
       (chicken::error 'assert "failed assertion" (quote ?expr))))))


;;;; exceptional-condition objects API
;;
;;This is meant to be functionally equivalent to the API defined by "(rnrs conditions (6))".
;;

(define-record <condition>
  name
		;A symbol representing the condition kind name.
  parent
		;The  parent "<condition>"  object.  Set  to #f  if this  type has  no parent;  only
		;"&condition" has no parent.
  constructor
		;The exceptional-condition object-type constructor procedure.
  predicate
		;The exceptional-condition object-type predicate procedure.
  number-of-fields
		;A non-negative fixnum representing the number of fields for this type.
  total-number-of-fields
		;A non-negative fixnum representing  the number of fields for this  type and all its
		;subtypes.
  #| end of DEFINE-RECORD |# )

(define (condition . cnds)
  (if (null? cnds)
      (make-condition)
    (apply chicken::make-composite-condition cnds)))

(define condition?
  chicken::condition?)

(define (simple-conditions cnd)
  (map (lambda (C)
	 (apply chicken::make-property-condition C))
    (chicken::condition->list cnd)))

(define (condition-kinds cnd)
  (map (lambda (C)
	 (car C))
    (chicken::condition->list cnd)))

;;; --------------------------------------------------------------------

(define (make-condition)
  (chicken::make-property-condition '&condition))

(define &condition
  (make-<condition> '&condition #f make-condition condition? 0 0))


;;;; implementation of DEFINE-CONDITION-TYPE: helpers

(define (split-constructor-args type-spec parent-spec args)
  (let* ((parent-num	(<condition>-total-number-of-fields parent-spec))
	 (total-num	(<condition>-total-number-of-fields type-spec)))
    (unless (= total-num (length args))
      (chicken::abort
       (chicken::make-property-condition '&mmck-exceptional-conditions-wrong-constructor-arguments
					 'location (<condition>-name type-spec)
					 'message (format #f "wrong number of arguments to condition object constructor, \
                                                              expected ~a, got ~a"
							  total-num (length args))
					 'irritants args)))
    (let loop ((args		args)
	       (i			0)
	       (rev-parent-args	'())
	       (rev-type-args	'()))
      (cond ((null? args)
	     (values (reverse rev-parent-args) (reverse rev-type-args)))
	    ((< i parent-num)
	     (loop (cdr args) (+ 1 i) (cons (car args) rev-parent-args) rev-type-args))
	    (else
	     (loop (cdr args) (+ 1 i) rev-parent-args (cons (car args) rev-type-args)))))))

(define (zip-constructor-args field-names field-values)
  (if (null? field-names)
      '()
    (cons* (car field-names) (car field-values)
	   (zip-constructor-args (cdr field-names) (cdr field-values)))))


;;;; implementation of DEFINE-CONDITION-TYPE

(define-syntax define-condition-type
  (er-macro-transformer
    (lambda (input-form.stx rename compare)


;;;; syntax DEFINE-CONDITION-TYPE: helpers

(define (synner message . args)
  (apply syntax-error 'define-condition-type message input-form.stx args))

(define (synner-warning message . args)
  (apply warning message input-form.stx args))

(define (syntactic-identifier? stx)
  (symbol? stx))

(define (check-condition-type-name name.stx)
  (unless (syntactic-identifier? name.stx)
    (synner "expected syntactic identifier as exceptional-condition object-type name" name.stx)))

(define (check-condition-parent-type-name name.stx)
  (unless (syntactic-identifier? name.stx)
    (synner "expected syntactic identifier as exceptional-condition object-type parent name" name.stx)))

(define (check-condition-constructor-name name.stx)
  (unless (syntactic-identifier? name.stx)
    (synner "expected syntactic identifier as exceptional-condition object-type constructor name" name.stx)))

(define (check-condition-predicate-name name.stx)
  (unless (syntactic-identifier? name.stx)
    (synner "expected syntactic identifier as exceptional-condition object-type predicate name" name.stx)))

(define (check-condition-field-name name.stx)
  (unless (syntactic-identifier? name.stx)
    (synner "expected syntactic identifier as exceptional-condition object-type field name" name.stx)))

(define (check-condition-field-accessor-name name.stx)
  (unless (syntactic-identifier? name.stx)
    (synner "expected syntactic identifier as exceptional-condition object-type field accessor name" name.stx)))

(define (check-duplicate ell syn)
  (unless (null? ell)
    (let ((head (car ell))
	  (tail (cdr ell)))
      (if (exists (lambda (item)
		    (eq? item head))
	    tail)
	  (syn head)
	(check-duplicate tail syn)))))

(define (check-duplicate-field-name field-names)
  (check-duplicate field-names
		   (lambda (field-name.id)
		     (synner "duplicate field name in exceptional-condition object-type definition" field-name.id))))

(define (check-duplicate-field-accessor-name field-accessor-names)
  (check-duplicate field-accessor-names
		   (lambda (field-accessor.id)
		     (synner "duplicate field accessor name in exceptional-condition object-type definition" field-accessor.id))))


;;;; syntax DEFINE-CONDITION-TYPE: syntactic identifiers used in the output form

(define %begin		(rename 'begin))
(define %quote		(rename 'quote))
(define %define		(rename 'define))
(define %apply		(rename 'apply))
(define %receive	(rename 'receive))
(define %+		(rename '+))

(define %chicken::make-composite-condition	(rename 'chicken::make-composite-condition))
(define %chicken::make-property-condition	(rename 'chicken::make-property-condition))
(define %chicken::condition-predicate		(rename 'chicken::condition-predicate))
(define %chicken::condition-property-accessor	(rename 'chicken::condition-property-accessor))

(define %make-<condition>			(rename 'make-<condition>))
(define %<condition>-constructor		(rename '<condition>-constructor))
(define %<condition>-total-number-of-fields	(rename '<condition>-total-number-of-fields))

(define %split-constructor-args			(rename 'split-constructor-args))
(define %zip-constructor-args			(rename 'zip-constructor-args))


;;;; syntax DEFINE-CONDITION-TYPE: data types

(define-record <parsed-input>
  type-id
		;A syntactic identifier representing the exceptional-condition object-type name.
  parent-id
		;A syntactic  identifier representing  the exceptional-condition  object-type parent
		;name.
  constructor-id
		;A   syntactic  identifier   representing   the  exceptional-condition   object-type
		;constructor name.
  predicate-id
		;A   syntactic  identifier   representing   the  exceptional-condition   object-type
		;predicate name.
  fields
		;A (possibly empty) list of "<field>" records representing the field specifications.
  #| end of DEFINE-RECORD |# )

(define-record <field>
  name
		;The symbol name of a field.
  accessor
		;The symbol name of a field's accessor.
  #| end of DEFINE-RECORD |# )


;;;; syntax DEFINE-CONDITION-TYPE: parsing and building

(define (main input-form.stx)
  (let ((parsed-input (parse-input-form input-form.stx)))
    (build-output-form parsed-input)))

(define (parse-input-form input-form.stx)
  (match input-form.stx
    ((_ ?condition-type-name ?parent-condition-type-name ?constructor-id ?predicate-id (?field-name* ?field-accessor*) ...)
     (begin
       (check-condition-type-name		?condition-type-name)
       (check-condition-parent-type-name	?parent-condition-type-name)
       (check-condition-constructor-name	?constructor-id)
       (check-condition-predicate-name		?predicate-id)
       (for-each check-condition-field-name		?field-name*)
       (for-each check-condition-field-accessor-name	?field-accessor*)
       (check-duplicate-field-name		?field-name*)
       (check-duplicate-field-accessor-name	?field-accessor*)
       (make-<parsed-input> ?condition-type-name ?parent-condition-type-name ?constructor-id ?predicate-id
			    (map (lambda (field-name.id field-accessor.id)
				   (make-<field> field-name.id field-accessor.id))
			      ?field-name* ?field-accessor*))))
    (_
     (synner "syntax error in exceptional-condition object-type definition form"))))

(define (build-output-form parsed-input)
  (let ((TYPE-ID		(<parsed-input>-type-id		parsed-input))
	(PARENT-ID		(<parsed-input>-parent-id	parsed-input))
	(CONSTRUCTOR-ID		(<parsed-input>-constructor-id	parsed-input))
	(PREDICATE-ID		(<parsed-input>-predicate-id	parsed-input))
	(FIELD-NAME*		(map <field>-name (<parsed-input>-fields parsed-input))))
    (let ((NUMBER-OF-FIELDS	(length (<parsed-input>-fields parsed-input)))
	  (FIELD-NAMES		(map <field>-name (<parsed-input>-fields parsed-input)))
	  (FIELD-ACCESSORS	(map (lambda (field-spec)
				       (let ((FIELD-NAME	(<field>-name     field-spec))
					     (FIELD-ACCESSOR	(<field>-accessor field-spec)))
					 `(,%define ,FIELD-ACCESSOR
						    (,%chicken::condition-property-accessor (,%quote ,TYPE-ID) (,%quote ,FIELD-NAME)))))
				  (<parsed-input>-fields parsed-input))))
      `(,%begin
	(,%define (,CONSTRUCTOR-ID . args)
		  (,%receive (parent-args type-args)
			     (,%split-constructor-args ,TYPE-ID ,PARENT-ID args)
			     (,%chicken::make-composite-condition
			      (,%apply ,%chicken::make-property-condition (,%quote ,TYPE-ID)
				       (,%zip-constructor-args (,%quote ,FIELD-NAMES)
							       type-args))
			      (,%apply (,%<condition>-constructor ,PARENT-ID) parent-args))))

	(,%define ,PREDICATE-ID
		  (,%chicken::condition-predicate (,%quote ,TYPE-ID)))

	,@FIELD-ACCESSORS

	(,%define ,TYPE-ID
		  (,%make-<condition> (,%quote ,TYPE-ID) ,PARENT-ID ,CONSTRUCTOR-ID ,PREDICATE-ID
				      ,NUMBER-OF-FIELDS
				      (,%+ ,NUMBER-OF-FIELDS (,%<condition>-total-number-of-fields ,PARENT-ID))))

	#| end of %begin |# ))))


;;;; syntax DEFINE-CONDITION-TYPE: let's go

(main input-form.stx))))


;;;; exceptional-condition object-types defined by R6RS

(define-condition-type &who
    &condition
  $make-who-condition
  who-condition?
  (who	condition-who))

(define-condition-type &message
    &condition
  $make-message-condition
  message-condition?
  (message	condition-message))

(define-condition-type &irritants
    &condition
  $make-irritants-condition
  irritants-condition?
  (irritants	condition-irritants))

(define-condition-type &warning
    &condition
  make-warning
  warning?)

(define-condition-type &serious
    &condition
  make-serious-condition
  serious-condition?)

(define-condition-type &error
    &serious
  make-error
  error?)

(define-condition-type &violation
    &serious
  make-violation
  violation?)

(define-condition-type &assertion
    &violation
  make-assertion-violation
  assertion-violation?)

(define-condition-type &non-continuable
    &violation
  make-non-continuable-violation
  non-continuable-violation?)

(define-condition-type &implementation-restriction
    &violation
  make-implementation-restriction-violation
  implementation-restriction-violation?)

(define-condition-type &lexical
    &violation
  make-lexical-violation
  lexical-violation?)

(define-condition-type &syntax
    &violation
  make-syntax-violation
  syntax-violation?)

(define-condition-type &undefined
    &violation
  make-undefined-violation
  undefined-violation?)


;;;; raising special exceptions

(define (who-condition-value? obj)
  (or (symbol? obj)
      (string? obj)
      (not     obj)))

(define (message-condition-value? obj)
  (string? obj))

(define (irritants-condition-value? obj)
  (list? obj))


;;;; extended exceptional-condition object constructors

(define (make-who-condition who)
  (%internal-assert (who-condition-value? who))
  ($make-who-condition who))

(define (make-message-condition message)
  (%internal-assert (message-condition-value? message))
  ($make-message-condition message))

(define (make-irritants-condition irritants)
  (%internal-assert (irritants-condition-value? irritants))
  ($make-irritants-condition irritants))


;;;; done

#| end of module |# )

;;; end of file

;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: main compilation unit
;;;Date: Jul 13, 2019
;;;
;;;Abstract
;;;
;;;	This is the main compilation unit; it USES all the other compilation units.
;;;
;;;	This compilation units defines the main module:  it imports all the modules exporting public
;;;	syntactic bindings and it reexports all such syntactic bindings.
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

(declare (unit mmck.exceptional-conditions)
	 (uses mmck.exceptional-conditions.version)
	 (uses mmck.exceptional-conditions.condition-objects)
	 (uses mmck.exceptional-conditions.handlers)
	 (uses mmck.exceptional-conditions.compensations)
	 (uses mmck.exceptional-conditions.coroutines)
	 (uses mmck.exceptional-conditions.macros)
	 (emit-import-library mmck.exceptional-conditions))

(module (mmck.exceptional-conditions)
    ()
  (import (only (chicken module) reexport))
  (reexport (mmck.exceptional-conditions.handlers))
  (reexport (only (mmck.exceptional-conditions.unwind-protection)
		  unwinding-call/cc))
  (reexport (only (mmck.exceptional-conditions.condition-objects)
		  define-condition-type
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
		  &non-reinstatable make-non-reinstatable-violation non-reinstatable-violation?
		  ;;
		  condition
		  condition?
		  simple-conditions
		  condition-kinds))
  (reexport (only (mmck.exceptional-conditions.compensations)
		  run-compensations
		  push-compensation-thunk))
  (reexport (only (mmck.exceptional-conditions.coroutines)
		  coroutine
		  yield finish-coroutines
		  current-coroutine-uid coroutine-uid?
		  suspend-coroutine resume-coroutine suspended-coroutine?
		  reset-coroutines! dump-coroutines))
  (reexport (mmck.exceptional-conditions.macros))
  (reexport (mmck.exceptional-conditions.version))
  #| end of module |# )

;;; end of file

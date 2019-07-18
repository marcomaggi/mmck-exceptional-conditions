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
	 (uses mmck.exceptional-conditions.macros)
	 (emit-import-library mmck.exceptional-conditions))

(module (mmck.exceptional-conditions)
    ()
  (import (only (chicken module) reexport))
  (reexport (mmck.exceptional-conditions.handlers))
  (reexport (mmck.exceptional-conditions.condition-objects))
  (reexport (mmck.exceptional-conditions.macros))
  (reexport (mmck.exceptional-conditions.version))
  #| end of module |# )

;;; end of file

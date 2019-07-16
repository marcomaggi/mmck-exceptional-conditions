;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: exceptional condition handlers
;;;Date: Jul 16, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the exceptional condition handlers.
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

(declare (unit mmck.exceptional-conditions.handlers)
	 (uses mmck.exceptional-conditions.condition-objects)
	 (uses mmck.exceptional-conditions.helpers)
	 (emit-import-library mmck.exceptional-conditions.handlers))

(module (mmck.exceptional-conditions.handlers)
    ()
  (import (scheme)
	  (prefix (only (chicken condition)
			with-exception-handler)
		  chicken::)
	  (mmck exceptional-conditions helpers))
  (import (only (chicken module) reexport))
  (reexport (only (chicken condition)
		  with-exception-handler
		  condition-case))
  (import-for-syntax (scheme)
		     (only (chicken base)
			   define-record)
		     (only (chicken syntax)
			   er-macro-transformer)
		     (only (matchable)
			   match)
		     (mmck exceptional-conditions helpers))





;;;; done

#| end of module |# )

;;; end of file

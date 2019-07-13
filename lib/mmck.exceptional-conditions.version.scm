;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: version functions
;;;Date: Jul 13, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines version functions.
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

(declare (unit mmck.exceptional-conditions.version)
	 (emit-import-library mmck.exceptional-conditions.version))

(module (mmck.exceptional-conditions.version)
    (mmck-exceptional-conditions-package-major-version
     mmck-exceptional-conditions-package-minor-version
     mmck-exceptional-conditions-package-patch-level
     mmck-exceptional-conditions-package-prerelease-tag
     mmck-exceptional-conditions-package-build-metadata
     mmck-exceptional-conditions-package-version
     mmck-exceptional-conditions-package-semantic-version)
  (import (scheme)
	  (prefix mmck.exceptional-conditions.config config::))


;;;; version functions

(define (mmck-exceptional-conditions-package-major-version)	config::MMUX_PKG_MAJOR_VERSION)
(define (mmck-exceptional-conditions-package-minor-version)	config::MMUX_PKG_MINOR_VERSION)
(define (mmck-exceptional-conditions-package-patch-level)	config::MMUX_PKG_PATCH_LEVEL)
(define (mmck-exceptional-conditions-package-prerelease-tag)	config::MMUX_PKG_PRERELEASE_TAG)
(define (mmck-exceptional-conditions-package-build-metadata)	config::MMUX_PKG_BUILD_METADATA)
(define (mmck-exceptional-conditions-package-version)		config::MMUX_PKG_VERSION)
(define (mmck-exceptional-conditions-package-semantic-version)	config::MMUX_PKG_SEMANTIC_VERSION)


;;;; done

#| end of module |# )

;;; end of file

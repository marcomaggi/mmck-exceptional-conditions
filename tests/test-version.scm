;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Exceptional Conditions
;;;Contents: test program for version functions
;;;Date: Jul 13, 2019
;;;
;;;Abstract
;;;
;;;	This program tests version functions.
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

(require-library (mmck exceptional-conditions))

(module (test-version)
    ()
  (import (scheme)
	  (mmck exceptional-conditions)
	  (mmck checks)
	  (chicken pretty-print))

(check-set-mode! 'report-failed)
(check-display "*** testing version functions\n")


(parameterise ((check-test-name		'versions))

  (pretty-print (list 'mmck-exceptional-conditions-package-major-version	(mmck-exceptional-conditions-package-major-version)))
  (pretty-print (list 'mmck-exceptional-conditions-package-minor-version	(mmck-exceptional-conditions-package-minor-version)))
  (pretty-print (list 'mmck-exceptional-conditions-package-patch-level		(mmck-exceptional-conditions-package-patch-level)))
  (pretty-print (list 'mmck-exceptional-conditions-package-prerelease-tag	(mmck-exceptional-conditions-package-prerelease-tag)))
  (pretty-print (list 'mmck-exceptional-conditions-package-build-metadata	(mmck-exceptional-conditions-package-build-metadata)))
  (pretty-print (list 'mmck-exceptional-conditions-package-version		(mmck-exceptional-conditions-package-version)))
  (pretty-print (list 'mmck-exceptional-conditions-package-semantic-version	(mmck-exceptional-conditions-package-semantic-version)))

  (check-for-true		(number? (mmck-exceptional-conditions-package-major-version)))
  (check-for-true		(number? (mmck-exceptional-conditions-package-minor-version)))
  (check-for-true		(number? (mmck-exceptional-conditions-package-patch-level)))
  (check-for-true		(string? (mmck-exceptional-conditions-package-prerelease-tag)))
  (check-for-true		(string? (mmck-exceptional-conditions-package-build-metadata)))
  (check-for-true		(string? (mmck-exceptional-conditions-package-version)))
  (check-for-true		(string? (mmck-exceptional-conditions-package-semantic-version)))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file

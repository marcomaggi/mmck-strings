;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Strings
;;;Contents: test program for version functions
;;;Date: Aug 16, 2019
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

(module (test-version)
    ()
  (import (scheme)
	  (mmck strings)
	  (mmck checks)
	  (chicken pretty-print))

(check-set-mode! 'report-failed)
(check-display "*** testing strings: version functions\n")


(parameterise ((check-test-name		'versions))

  (pretty-print (list 'mmck-strings-package-major-version	(mmck-strings-package-major-version)))
  (pretty-print (list 'mmck-strings-package-minor-version	(mmck-strings-package-minor-version)))
  (pretty-print (list 'mmck-strings-package-patch-level		(mmck-strings-package-patch-level)))
  (pretty-print (list 'mmck-strings-package-prerelease-tag	(mmck-strings-package-prerelease-tag)))
  (pretty-print (list 'mmck-strings-package-build-metadata	(mmck-strings-package-build-metadata)))
  (pretty-print (list 'mmck-strings-package-version		(mmck-strings-package-version)))
  (pretty-print (list 'mmck-strings-package-semantic-version	(mmck-strings-package-semantic-version)))

  (check-for-true		(number? (mmck-strings-package-major-version)))
  (check-for-true		(number? (mmck-strings-package-minor-version)))
  (check-for-true		(number? (mmck-strings-package-patch-level)))
  (check-for-true		(string? (mmck-strings-package-prerelease-tag)))
  (check-for-true		(string? (mmck-strings-package-build-metadata)))
  (check-for-true		(string? (mmck-strings-package-version)))
  (check-for-true		(string? (mmck-strings-package-semantic-version)))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file

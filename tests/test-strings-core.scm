;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Strings
;;;Contents: test program for demo
;;;Date: Aug 16, 2019
;;;
;;;Abstract
;;;
;;;	This program is a demo of the features.
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

(module (test-strings-core)
    ()
  (import (scheme)
	  (only (chicken base)
		add1
		void)
	  (mmck strings)
	  (mmck exceptional-conditions)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing strings handling: core functions\n")


(parameterise ((check-test-name		'unsafe))

  (check
      (let ((str	"123"))
	(values ($string-length str)
		;; ($string-ref str 0)
		;; ($string-ref str 1)
		;; ($string-ref str 2)
		))
    => 3)

  (values))


(parameterise ((check-test-name		'predicates))

  (check-for-true	(list-of-strings? '()))
  (check-for-true	(list-of-strings? '("a")))
  (check-for-true	(list-of-strings? '("a" "b")))
  ;;
  (check-for-false	(list-of-strings? ""))
  (check-for-false	(list-of-strings? '("a" 123)))
  (check-for-false	(list-of-strings? 123))

;;; --------------------------------------------------------------------

  (check-for-true	(list-of-strings-of-equal-length? '()))
  (check-for-true	(list-of-strings-of-equal-length? '("a")))
  (check-for-true	(list-of-strings-of-equal-length? '("a" "b")))

  (check-for-false	(list-of-strings-of-equal-length? '("a" (b))))
  (check-for-false	(list-of-strings-of-equal-length? '("a" "ab")))

  (check-for-true	(list-of-strings-of-equal-length? '("a" "b" "c")))
  (check-for-false	(list-of-strings-of-equal-length? '("a" "b" "c99")))

  (check-for-true	(list-of-strings-of-equal-length? '("a" "b" "c" "d")))
  (check-for-false	(list-of-strings-of-equal-length? '("a" "b" "c" "d99")))

;;; --------------------------------------------------------------------

  (check-for-true	(strings-of-equal-length? ""))
  (check-for-true	(strings-of-equal-length? '"a"))
  (check-for-true	(strings-of-equal-length? '"a" '"b"))

  (check-for-false	(strings-of-equal-length? '"a" '(b)))
  (check-for-false	(strings-of-equal-length? '"a" '"ab"))

  (check-for-true	(strings-of-equal-length? "a" "b" "c"))
  (check-for-false	(strings-of-equal-length? "a" "b" "c99"))

  (check-for-true	(strings-of-equal-length? "a" "b" "c" "d"))
  (check-for-false	(strings-of-equal-length? "a" "b" "c" "d99"))

  (values))


(parameterise ((check-test-name		'fold-left))

  (check
      (string-fold-left
	  (lambda (knil item)
	    (cons item knil))
	123
	"")
    => 123)

  (check
      (string-fold-left
	  (lambda (knil item)
	    (cons item knil))
	'(0)
	"abc")
    => '(#\c #\b #\a 0))

;;; --------------------------------------------------------------------

  (check
      (string-fold-left
	  (lambda (knil item1 item2)
	    (cons (list item1 item2) knil))
	123
	""
	"")
    => 123)

  (check
      (string-fold-left
	  (lambda (knil item1 item2)
	    (cons (list item1 item2) knil))
	'(0)
	"abc"
	"def")
    => '((#\c #\f) (#\b #\e) (#\a #\d) 0))

;;; --------------------------------------------------------------------

  (check
      (string-fold-left
	  (lambda (knil item1 item2 item3)
	    (cons (list item1 item2 item3) knil))
	123
	""
	""
	"")
    => 123)

  (check
      (string-fold-left
	  (lambda (knil item1 item2 item3)
	    (cons (list item1 item2 item3) knil))
	'(0)
	"abc"
	"def"
	"ghi")
    => '((#\c #\f #\i) (#\b #\e #\h) (#\a #\d #\g) 0))

;;; --------------------------------------------------------------------

  (check
      (string-fold-left
	  (lambda (knil item1 item2 item3 item4)
	    (cons (list item1 item2 item3 item4) knil))
	123
	""
	""
	""
	"")
    => 123)

  (check
      (string-fold-left
	  (lambda (knil item1 item2 item3 item4)
	    (cons (list item1 item2 item3 item4) knil))
	'(0)
	"abc"
	"def"
	"ghi"
	"lmn")
    => '((#\c #\f #\i #\n) (#\b #\e #\h #\m) (#\a #\d #\g #\l) 0))

  (values))


(parameterise ((check-test-name		'fold-right))

  (check
      (string-fold-right
	  (lambda (item knil)
	    (cons item knil))
	123
	"")
    => 123)

  (check
      (string-fold-right
	  (lambda (item knil)
	    (cons item knil))
	'(0)
	"abc")
    => '(#\a #\b #\c 0))

;;; --------------------------------------------------------------------

  (check
      (string-fold-right
	  (lambda (item1 item2 knil)
	    (cons (list item1 item2) knil))
	123
	""
	"")
    => 123)

  (check
      (string-fold-right
	  (lambda (item1 item2 knil)
	    (cons (list item1 item2) knil))
	'(0)
	"abc"
	"def")
    => '((#\a #\d) (#\b #\e) (#\c #\f) 0))

;;; --------------------------------------------------------------------

  (check
      (string-fold-right
	  (lambda (item1 item2 item3 knil)
	    (cons (list item1 item2 item3) knil))
	123
	""
	""
	"")
    => 123)

  (check
      (string-fold-right
	  (lambda (item1 item2 item3 knil)
	    (cons (list item1 item2 item3) knil))
	'(0)
	"abc"
	"def"
	"ghi")
    => '((#\a #\d #\g) (#\b #\e #\h) (#\c #\f #\i) 0))

;;; --------------------------------------------------------------------

  (check
      (string-fold-right
	  (lambda (item1 item2 item3 item4 knil)
	    (cons (list item1 item2 item3 item4) knil))
	123
	""
	""
	""
	"")
    => 123)

  (check
      (string-fold-right
	  (lambda (item1 item2 item3 item4 knil)
	    (cons (list item1 item2 item3 item4) knil))
	'(0)
	"abc"
	"def"
	"ghi"
	"lmn")
    => '((#\a #\d #\g #\l)
	 (#\b #\e #\h #\m)
	 (#\c #\f #\i #\n)
	 0))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file

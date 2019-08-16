;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Strings
;;;Contents: assertions module
;;;Date: Aug 16, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines more facilities to handle assertions.
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

(declare (unit mmck.strings.assertions)
	 (emit-import-library mmck.strings.assertions))

(module (mmck.strings.assertions)
    ((syntax: assert* assertion-violation)
     assert-argument-type
     assert-argument-type/rest
     assert-argument-list-of-type
     assert-argument-vector-of-type)
  (import (scheme)
	  (only (chicken base)
		unless)
	  (only (chicken format)
		format)
	  (mmck lang)
	  (mmck exceptional-conditions))
  (import-for-syntax (scheme)
		     (only (chicken syntax)
			   #;ir-macro-transformer
			   er-macro-transformer
			   syntax-error)
		     (only (matchable)
			   match))


;;;; helpers

(define (fold-left combine nil ell)
  (if (pair? ell)
      (fold-left combine (combine nil (car ell)) (cdr ell))
    nil))


;;;; assertions

(define-syntax assert*
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (match input-form.stx
	((_ ?expr)
	 (let ((%unless			(rename 'unless))
	       (%quote			(rename 'quote))
	       (%assertion-violation	(rename 'unless))
	       (%__who__		(rename '__who__)))
	   `(,%unless ,?expr (,%assertion-violation (__who__) "failed assertion" (,%quote ,?expr)))))
	(_
	 (syntax-error 'assert* "syntax error in macro use" input-form.stx))))))


;;;; predicates

(define (assert-argument-type who type.str type-pred arg arg.idx)
  ;;Usage example:
  ;;
  ;;  (define* (fold-left combine ell)
  ;;    (assert-argument-type (__who__) "procedure" procedure? combine 1)
  ;;    (assert-argument-type (__who__) "list"      list?      ell     2)
  ;;    ---)
  ;;
  (unless (type-pred arg)
    (assertion-violation who
      (format #f "expected argument ~a of type \"~a\"" arg.idx type.str)
      arg)))

(define (assert-argument-type/rest who type.str type-pred rest-arg)
  ;;Usage example:
  ;;
  ;;  (define* (fold-left combine ell . ell*)
  ;;    (assert-argument-type (__who__) "procedure" procedure? combine 1)
  ;;    (assert-argument-type (__who__) "list"      list?      ell     2)
  ;;    (assert-argument-type/rest (__who__) "list of strings" list-of-strings? ell*)
  ;;    ---)
  ;;
  (unless (type-pred rest-arg)
    (assertion-violation who
      (format #f "expected rest argument of type \"~a\"" type.str)
      rest-arg)))

(define (assert-argument-list-of-type who type.str type-pred arg* arg.idx)
  (fold-left (lambda (item.idx item)
	       (if (type-pred item)
		   (+ 1 item.idx)
		 (assertion-violation who
		   (string-append "expected item of type \"" type.str "\""
				  " at index " (number->string item.idx)
				  " of list argument " (number->string arg.idx))
		   item)))
    0 arg*))

(define (assert-argument-vector-of-type who type.str type-pred arg-vec arg.idx)
  (unless (vector? arg-vec)
    (assertion-violation who
      (string-append "expected vector as argument \""
		     type.str
		     "\" at index "
		     (number->string arg.idx))
      arg-vec))
  (do ((i 0 (+ 1 i)))
      ((= i (vector-length arg-vec)))
    (unless (type-pred (vector-ref arg-vec i))
      (assertion-violation who
	(string-append "expected item of type \"" type.str "\""
		       " at index " (number->string i)
		       " of list argument " )
	arg-vec
	(vector-ref arg-vec i)))))


;;;; done

#| end of module |# )

;;; end of file

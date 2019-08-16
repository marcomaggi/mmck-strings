;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Strings
;;;Contents: core strings handling functions
;;;Date: Aug 16, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the core strings handling functions.
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

(declare (unit mmck.strings.core)
	 (uses mmck.strings.assertions)
	 (emit-import-library mmck.strings.core))

(module (mmck.strings.core)
    (
     ;; unsafe operations
     (syntax: $string-length)

     ;; constructors

     ;; predicates
     string-empty?
     string-not-empty?
     list-of-strings?
     strings-of-equal-length?
     list-of-strings-of-equal-length?

     ;; iteration and searching
     string-fold-left string-fold-right
     string-find string-exists string-for-all
     string-map string-for-each
     string-map-in-order string-for-each-in-order
     string-map-index string-for-each-index

     ;; unsafe iteration and searching
     $string-fold-left/1
     $string-fold-left/2
     $string-fold-left/3
     $string-fold-left/list
     ;;
     $string-fold-right/1
     $string-fold-right/2
     $string-fold-right/3
     $string-fold-right/list
     ;;
     $string-map/1
     $string-map/2
     $string-map/3
     $string-map/list
     ;;
     $string-for-each/1
     $string-for-each/2
     $string-for-each/3
     $string-for-each/list
     ;;
     $string-map-in-order/1
     $string-map-in-order/2
     $string-map-in-order/3
     $string-map-in-order/list
     ;;
     $string-for-each-in-order/1
     $string-for-each-in-order/2
     $string-for-each-in-order/3
     $string-for-each-in-order/list
     ;;
     $string-map-index/1
     $string-map-index/2
     $string-map-index/3
     $string-map-index/list
     ;;
     $string-for-each-index/1
     $string-for-each-index/2
     $string-for-each-index/3
     $string-for-each-index/list
     ;;
     $string-for-all/1
     $string-for-all/2
     $string-for-all/3
     $string-for-all/list
     ;;
     $string-exists/1
     $string-exists/2
     $string-exists/3
     $string-exists/list
     ;;
     $string-find

     ;; copying
     string-copy!
     $string-copy!

     ;; miscellaneous
     sorted-string-binary-search

     ;; exceptional-condition object-types
     &string-is-empty
     make-string-is-empty-condition
     condition-string-is-empty?
     raise-exception-string-is-empty
     ;;
     &strings-are-of-different-length
     make-strings-are-of-different-length-condition
     condition-strings-are-of-different-length?
     raise-exception-strings-are-of-different-length
     ;;
     &strings-are-empty-or-of-different-length
     make-strings-are-empty-or-of-different-length-condition
     condition-strings-are-empty-or-of-different-length?
     raise-exception-strings-are-empty-or-of-different-length
     )
  (import (scheme)
	  (only (chicken type)
		:)
	  (only (chicken base)
		add1
		sub1
		call/cc
		fixnum?
		void
		when)
	  (only (chicken fixnum)
		fxshr)
	  (mmck strings assertions)
	  (mmck lang)
	  (mmck exceptional-conditions))


;;;; lists handling

(case-define cons*
  ((item)
   item)
  ((item ell)
   (cons item ell))
  ((item1 item2 ell)
   (cons item1 (cons item2 ell)))
  ((item . rest)
   (let loop ((item	item)
	      (rest	rest))
     (if (null? rest)
	 item
       (cons item (loop (car rest) (cdr rest)))))))

(define ($for-all/1 pred ell)
  (or (null? ell)
      (if (null? (cdr ell))
	  ;;Perform a tail call for the last item.
	  (if (pred (car ell))
	      #t
	    #f)
	(and (pred (car ell))
	     ($for-all/1 pred (cdr ell))))))

(define ($fold-left/1 combine knil ell)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (pair? ell)
      (if (null? (cdr ell))
	  ;;Perform a tail call to COMBINE for the last element.
	  (combine knil (car ell))
	($fold-left/1 combine (combine knil (car ell)) (cdr ell)))
    knil))

(define ($fold-right/1 combine knil ell)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((knil	knil)
	     (rev-ell	(reverse ell)))
    (if (pair? rev-ell)
	(if (null? (cdr rev-ell))
	    ;;Perform a tail call to COMBINE for the last element.
	    (combine (car rev-ell) knil)
	  (loop (combine (car rev-ell) knil)
		(cdr rev-ell)))
      knil)))

(define ($map/1 func ell)
  ($fold-right/1 (lambda (item nil)
		   (cons (func item) nil))
    '() ell))


;;;; unsafe operations

(define-syntax-rule ($string-length ?string)
  ;;Unsafe implementation  of STRING-LENGTH.   To be  used when we  know that:  ?STRING is  a string
  ;;object.
  ;;
  (##sys#size ?string))


;;;; helpers

(define-syntax define-string-folder
  (syntax-rules ()
    ((_ ?who ?string-folder/1 ?string-folder/2 ?string-folder/3 ?string-folder/list)
     (case-define ?who
       ((combine nil vec)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "string"      string?      vec     2))
	(?string-folder/1 combine nil vec))

       ((combine nil str1 str2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1    2)
	  (assert-argument-type (quote ?who) "string"      string?      str2    3)
	  (assert-strings-of-equal-length (quote ?who) str1 str2))
	(?string-folder/2 combine nil str1 str2))

       ((combine nil str1 str2 str3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1    2)
	  (assert-argument-type (quote ?who) "string"      string?      str2    3)
	  (assert-argument-type (quote ?who) "string"      string?      str3    4)
	  (assert-strings-of-equal-length (quote ?who) str1 str2 str3))
	(?string-folder/3 combine nil str1 str2 str3))

       ((combine nil str1 str2 str3 . vec*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1    2)
	  (assert-argument-type (quote ?who) "string"      string?      str2    3)
	  (assert-argument-type (quote ?who) "string"      string?      str3    4)
	  (assert-argument-type/rest (quote ?who) "string of strings" list-of-strings? vec*)
	  (assert-strings-of-equal-length (quote ?who) str1 str2 str3 vec*))
	(?string-folder/list combine nil (cons* str1 str2 str3 vec*)))))
    ))

(define-syntax define-string-mapper
  (syntax-rules ()
    ((_ ?who ?string-mapper/1 ?string-mapper/2 ?string-mapper/3 ?string-mapper/list)
     (case-define ?who
       ((func vec)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "string"      string?      vec  2))
	(?string-mapper/1 func vec))

       ((func str1 str2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1 2)
	  (assert-argument-type (quote ?who) "string"      string?      str2 3)
	  (assert-strings-of-equal-length (quote ?who) str1 str2))
	(?string-mapper/2 func str1 str2))

       ((func str1 str2 str3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1 2)
	  (assert-argument-type (quote ?who) "string"      string?      str2 3)
	  (assert-argument-type (quote ?who) "string"      string?      str3 4)
	  (assert-strings-of-equal-length (quote ?who) str1 str2 str3))
	(?string-mapper/3 func str1 str2 str3))

       ((func str1 str2 str3 . vec*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1 2)
	  (assert-argument-type (quote ?who) "string"      string?      str2 3)
	  (assert-argument-type (quote ?who) "string"      string?      str3 4)
	  (assert-argument-type/rest (quote ?who) "string of strings" list-of-strings? vec*)
	  (assert-strings-of-equal-length (quote ?who) str1 str2 str3 vec*))
	(?string-mapper/list func (cons* str1 str2 str3 vec*)))))
    ))

(define-syntax define-string-searcher
  (syntax-rules ()
    ((_ ?who ?string-searcher/1 ?string-searcher/2 ?string-searcher/3 ?string-searcher/list)
     (case-define ?who
       ((pred vec)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "string"      string?      vec  2))
	(?string-searcher/1 pred vec))

       ((pred str1 str2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1 2)
	  (assert-argument-type (quote ?who) "string"      string?      str2 3)
	  (assert-strings-of-equal-length (quote ?who) str1 str2))
	(?string-searcher/2 pred str1 str2))

       ((pred str1 str2 str3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1 2)
	  (assert-argument-type (quote ?who) "string"      string?      str2 3)
	  (assert-argument-type (quote ?who) "string"      string?      str3 4)
	  (assert-strings-of-equal-length (quote ?who) str1 str2 str3))
	(?string-searcher/3 pred str1 str2 str3))

       ((pred str1 str2 str3 . vec*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "string"      string?      str1 2)
	  (assert-argument-type (quote ?who) "string"      string?      str2 3)
	  (assert-argument-type (quote ?who) "string"      string?      str3 4)
	  (assert-argument-type/rest (quote ?who) "string of strings" list-of-strings? vec*)
	  (assert-strings-of-equal-length (quote ?who) str1 str2 str3 vec*))
	(?string-searcher/list pred (cons* str1 str2 str3 vec*)))))
    ))


;;;; exceptional-condition object-types

(define-condition-type &strings-are-of-different-length
    &assertion
  make-strings-are-of-different-length-condition
  condition-strings-are-of-different-length?)

(define (raise-exception-strings-are-of-different-length who string-of-strings)
  (raise
   (condition (make-strings-are-of-different-length-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid arguments, strings are of different length")
	      (make-irritants-condition (list string-of-strings)))))

;;; --------------------------------------------------------------------

(define-condition-type &string-is-empty
    &assertion
  make-string-is-empty-condition
  condition-string-is-empty?)

(define (raise-exception-string-is-empty who obj)
  (raise
   (condition (make-string-is-empty-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid operand, expected non-empty string")
	      (make-irritants-condition (list obj)))))

;;; --------------------------------------------------------------------

(define-condition-type &strings-are-empty-or-of-different-length
    &assertion
  make-strings-are-empty-or-of-different-length-condition
  condition-strings-are-empty-or-of-different-length?)

(define (raise-exception-strings-are-empty-or-of-different-length who string-of-strings)
  (raise
   (condition (make-strings-are-empty-or-of-different-length-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid arguments, strings are empty or of different length")
	      (make-irritants-condition (list string-of-strings)))))


;;;; constructors



;;;; predicates

(define (string-empty? obj)
  (and (string? obj)
       (zero? (string-length obj))))

(define (string-not-empty? obj)
  (and (string? obj)
       (positive? (string-length obj))))

(define (list-of-strings? objs)
  ;;Return true if OBJS is a (possibly empty)  list of strings; otherwise return false.  Notice that
  ;;this function returns false if OBJS is not null or a proper list of pairs.
  ;;
  (or (null? objs)
      (and (pair? objs)
	   (string? (car objs))
	   (list-of-strings? (cdr objs)))))

(define (list-of-strings-of-equal-length? str*)
  ;;Return true if STR*  is a list of strings of equal length;  otherwise return false.  Notice that
  ;;this function returns false if STR* is not null or a proper list of pairs.
  ;;
  (or (null? str*)
      (and (pair? str*)
	   (string? (car str*))
	   (let loop ((len1 (string-length (car str*)))
		      (str* (cdr str*)))
	     (or (null? str*)
		 (and (pair? str*)
		      (string? (car str*))
		      (= len1 (string-length (car str*)))
		      (loop len1 (cdr str*))))))))

(case-define strings-of-equal-length?
  ;;Return true if  all the arguments are  strings of equal length; otherwise  return false.  Notice
  ;;that this function returns false if one of the arguments is not a string.
  ;;
  ((str1)
   (string? str1))

  ((str1 str2)
   (and (string? str1)
	(string? str2)
	(= (string-length str1)
	   (string-length str2))))

  ((str1 str2 str3)
   (and (string? str1)
	(string? str2)
	(string? str3)
	(= (string-length str1)
	   (string-length str2)
	   (string-length str3))))


  ((str1 str2 str3 . str*)
   (and (string? str1)
	(string? str2)
	(string? str3)
	(let ((str.len (string-length str1)))
	  (and (= str.len
		  (string-length str2)
		  (string-length str3))
	       ($for-all/1 (lambda (str)
			     (= str.len (string-length str)))
		 str*)))))
  #| end of CASE-DEFINE |# )


;;;; special exceptional-condition raisers

(case-define assert-strings-of-equal-length
  ((who str1 str2)
   (unless (strings-of-equal-length? str1 str2)
     (raise-exception-strings-are-of-different-length who (list str1 str2))))
  ((who str1 str2 str3)
   (unless (strings-of-equal-length? str1 str2 str3)
     (raise-exception-strings-are-of-different-length who (list str1 str2 str3))))
  ((who str1 str2 str3 str*)
   (unless (list-of-strings-of-equal-length? (cons* str1 str2 str3 str*))
     (raise-exception-strings-are-of-different-length who (cons* str1 str2 str3 str*)))))


;;;; folding functions

(define ($string-fold-left/1 combine knil vec)
  (let ((vec.len (string-length vec)))
    (if (zero? vec.len)
	knil
      (let loop ((i	0)
		 (i+1	1)
		 (knil	knil))
	(if (= i+1 vec.len)
	    ;;Last call to COMBINE is in tail position.
	    (combine knil (string-ref vec i))
	  (loop i+1 (+ 1 i+1)
		(combine knil (string-ref vec i))))))))

(define ($string-fold-left/2 combine knil str1 str2)
  (let ((vec.len (string-length str1)))
    (if (zero? vec.len)
	knil
      (let loop ((i	0)
		 (i+1	1)
		 (knil	knil))
	(if (= i+1 vec.len)
	    ;;Last call to COMBINE is in tail position.
	    (combine knil (string-ref str1 i) (string-ref str2 i))
	  (loop i+1 (+ 1 i+1)
		(combine knil (string-ref str1 i) (string-ref str2 i))))))))

(define ($string-fold-left/3 combine knil str1 str2 str3)
  (let ((vec.len (string-length str1)))
    (if (zero? vec.len)
	knil
      (let loop ((i	0)
		 (i+1	1)
		 (knil	knil))
	(if (= i+1 vec.len)
	    ;;Last call to COMBINE is in tail position.
	    (combine knil (string-ref str1 i) (string-ref str2 i) (string-ref str3 i))
	  (loop i+1 (+ 1 i+1)
		(combine knil (string-ref str1 i) (string-ref str2 i) (string-ref str3 i))))))))

(define ($string-fold-left/list combine knil str*)
  (let ((vec.len (string-length (car str*))))
    (if (zero? vec.len)
	knil
      (let loop ((i	0)
		 (i+1	1)
		 (knil	knil))
	(if (= i+1 vec.len)
	    ;;Last call to COMBINE is in tail position.
	    (apply combine knil ($map/1 (lambda (vec)
					  (string-ref vec i))
				  str*))
	  (loop i+1 (+ 1 i+1)
		(apply combine knil ($map/1 (lambda (vec)
					      (string-ref vec i))
				      str*))))))))

;;; --------------------------------------------------------------------

(define ($string-fold-right/1 combine knil vec)
  (let ((vec.len (string-length vec)))
    (if (zero? vec.len)
	knil
      (let loop ((i	(sub1 vec.len))
		 (knil	knil))
	(if (zero? i)
	    ;;Last call to COMBINE is in tail position.
	    (combine (string-ref vec i) knil)
	  (loop (sub1 i) (combine (string-ref vec i) knil)))))))

(define ($string-fold-right/2 combine knil str1 str2)
  (let ((vec.len (string-length str1)))
    (if (zero? vec.len)
	knil
      (let loop ((i	(sub1 vec.len))
		 (knil	knil))
	(if (zero? i)
	    ;;Last call to COMBINE is in tail position.
	    (combine (string-ref str1 i) (string-ref str2 i) knil)
	  (loop (sub1 i) (combine (string-ref str1 i) (string-ref str2 i) knil)))))))

(define ($string-fold-right/3 combine knil str1 str2 str3)
  (let ((vec.len (string-length str1)))
    (if (zero? vec.len)
	knil
      (let loop ((i	(sub1 vec.len))
		 (knil	knil))
	(if (zero? i)
	    ;;Last call to COMBINE is in tail position.
	    (combine (string-ref str1 i) (string-ref str2 i) (string-ref str3 i) knil)
	  (loop (sub1 i)
		(combine (string-ref str1 i) (string-ref str2 i) (string-ref str3 i) knil)))))))

(define ($string-fold-right/list combine knil str*)
  (let ((vec.len (string-length (car str*))))
    (if (zero? vec.len)
	knil
      (let loop ((i	(sub1 vec.len))
		 (knil	knil))
	(if (zero? i)
	    ;;Last call to COMBINE is in tail position.
	    (apply combine (append ($map/1 (lambda (vec)
					     (string-ref vec i))
				     str*)
				   (list knil)))
	  (loop (sub1 i)
		(apply combine (append ($map/1 (lambda (vec)
						 (string-ref vec i))
					 str*)
				       (list knil)))))))))

;;; --------------------------------------------------------------------

(define-string-folder string-fold-left
  $string-fold-left/1
  $string-fold-left/2
  $string-fold-left/3
  $string-fold-left/list)

(define-string-folder string-fold-right
  $string-fold-right/1
  $string-fold-right/2
  $string-fold-right/3
  $string-fold-right/list)


;;;; string-mapping functions

(define ($string-map/1 func vec.in)
  (receive-and-return (vec.out)
      (make-string (string-length vec.in))
    ($string-fold-left/1 (lambda (idx item)
			   (string-set! vec.out idx (func item))
			   (add1 idx))
      0 vec.in)))

(define ($string-map/2 func str1 str2)
  (receive-and-return (vec.out)
      (make-string (string-length str1))
    ($string-fold-left/2 (lambda (idx item1 item2)
			   (string-set! vec.out idx (func item1 item2))
			   (add1 idx))
      0 str1 str2)))

(define ($string-map/3 func str1 str2 str3)
  (receive-and-return (vec.out)
      (make-string (string-length str1))
    ($string-fold-left/3 (lambda (idx item1 item2 item3)
			   (string-set! vec.out idx (func item1 item2 item3))
			   (add1 idx))
      0 str1 str2 str3)))

(define ($string-map/list func str*)
  (receive-and-return (vec.out)
      (make-string (string-length (car str*)))
    ($string-fold-left/list (lambda (idx . item*)
			      (string-set! vec.out idx (apply func item*))
			      (add1 idx))
      0 str*)))

;;; --------------------------------------------------------------------

(define ($string-for-each/1 func vec)
  ($string-fold-left/1 (lambda (idx item)
			 (func item)
			 (add1 idx))
    0 vec))

(define ($string-for-each/2 func str1 str2)
  ($string-fold-left/2 (lambda (idx item1 item2)
			 (func item1 item2)
			 (add1 idx))
    0 str1 str2))

(define ($string-for-each/3 func str1 str2 str3)
  ($string-fold-left/3 (lambda (idx item1 item2 item3)
			 (func item1 item2 item3)
			 (add1 idx))
    0 str1 str2 str3))

(define ($string-for-each/list func str*)
  ($string-fold-left/list (lambda (idx . item*)
			    (apply func item*)
			    (add1 idx))
    0 str*))

;;; --------------------------------------------------------------------

(define $string-map-in-order/1		$string-map/1)
(define $string-map-in-order/2		$string-map/2)
(define $string-map-in-order/3		$string-map/3)
(define $string-map-in-order/list	$string-map/list)

;;; --------------------------------------------------------------------

(define $string-for-each-in-order/1	$string-for-each/1)
(define $string-for-each-in-order/2	$string-for-each/2)
(define $string-for-each-in-order/3	$string-for-each/3)
(define $string-for-each-in-order/list	$string-for-each/list)

;;; --------------------------------------------------------------------

(define-string-mapper string-map
  $string-map/1
  $string-map/2
  $string-map/3
  $string-map/list)

(define-string-mapper string-for-each
  $string-for-each/1
  $string-for-each/2
  $string-for-each/3
  $string-for-each/list)

(define-string-mapper string-map-in-order
  $string-map-in-order/1
  $string-map-in-order/2
  $string-map-in-order/3
  $string-map-in-order/list)

(define-string-mapper string-for-each-in-order
  $string-for-each-in-order/1
  $string-for-each-in-order/2
  $string-for-each-in-order/3
  $string-for-each-in-order/list)


;;;; string-map-indexping functions

(define ($string-map-index/1 func vec.in)
  (receive-and-return (vec.out)
      (make-string (string-length vec.in))
    ($string-fold-left/1 (lambda (idx item)
			   (string-set! vec.out idx (func idx item))
			   (add1 idx))
      0 vec.in)))

(define ($string-map-index/2 func str1 str2)
  (receive-and-return (vec.out)
      (make-string (string-length str1))
    ($string-fold-left/2 (lambda (idx item1 item2)
			   (string-set! vec.out idx (func idx item1 item2))
			   (add1 idx))
      0 str1 str2)))

(define ($string-map-index/3 func str1 str2 str3)
  (receive-and-return (vec.out)
      (make-string (string-length str1))
    ($string-fold-left/3 (lambda (idx item1 item2 item3)
			   (string-set! vec.out idx (func idx item1 item2 item3))
			   (add1 idx))
      0 str1 str2 str3)))

(define ($string-map-index/list func str*)
  (receive-and-return (vec.out)
      (make-string (string-length (car str*)))
    ($string-fold-left/list (lambda (idx . item*)
			      (string-set! vec.out idx (apply func idx item*))
			      (add1 idx))
      0 str*)))

;;; --------------------------------------------------------------------

(define ($string-for-each-index/1 func vec)
  ($string-fold-left/1 (lambda (idx item)
			 (func idx item)
			 (add1 idx))
    0 vec))

(define ($string-for-each-index/2 func str1 str2)
  ($string-fold-left/2 (lambda (idx item1 item2)
			 (func idx item1 item2)
			 (add1 idx))
    0 str1 str2))

(define ($string-for-each-index/3 func str1 str2 str3)
  ($string-fold-left/3 (lambda (idx item1 item2 item3)
			 (func idx item1 item2 item3)
			 (add1 idx))
    0 str1 str2 str3))

(define ($string-for-each-index/list func str*)
  ($string-fold-left/list (lambda (idx . item*)
			    (apply func idx item*)
			    (add1 idx))
    0 str*))

;;; --------------------------------------------------------------------

(define-string-mapper string-map-index
  $string-map-index/1
  $string-map-index/2
  $string-map-index/3
  $string-map-index/list)

(define-string-mapper string-for-each-index
  $string-for-each-index/1
  $string-for-each-index/2
  $string-for-each-index/3
  $string-for-each-index/list)


;;;; search functions

(define ($string-for-all/1 pred vec)
  (call/cc
      (lambda (escape)
	($string-fold-left/1 (lambda (knil item)
			       (if (pred item)
				   knil
				 (escape #f)))
	  #t vec))))

(define ($string-for-all/2 pred str1 str2)
  (call/cc
      (lambda (escape)
	($string-fold-left/2 (lambda (knil item1 item2)
			       (if (pred item1 item2)
				   knil
				 (escape #f)))
	  #t str1 str2))))

(define ($string-for-all/3 pred str1 str2 str3)
  (call/cc
      (lambda (escape)
	($string-fold-left/3 (lambda (knil item1 item2 item3)
			       (if (pred item1 item2 item3)
				   knil
				 (escape #f)))
	  #t str1 str2 str3))))

(define ($string-for-all/list pred str*)
  (call/cc
      (lambda (escape)
	($string-fold-left/list (lambda (knil . item*)
				  (if (apply pred item*)
				      knil
				    (escape #f)))
	  #t str*))))

;;; --------------------------------------------------------------------

(define ($string-exists/1 pred vec)
  (call/cc
      (lambda (escape)
	($string-fold-left/1 (lambda (knil item)
			       (cond ((pred item)
				      => escape)
				     (else
				      knil)))
	  #f vec))))

(define ($string-exists/2 pred str1 str2)
  (call/cc
      (lambda (escape)
	($string-fold-left/2 (lambda (knil item1 item2)
			       (cond ((pred item1 item2)
				      => escape)
				     (else
				      knil)))
	  #f str1 str2))))

(define ($string-exists/3 pred str1 str2 str3)
  (call/cc
      (lambda (escape)
	($string-fold-left/3 (lambda (knil item1 item2 item3)
			       (cond ((pred item1 item2 item3)
				      => escape)
				     (else
				      knil)))
	  #f str1 str2 str3))))

(define ($string-exists/list pred str*)
  (call/cc
      (lambda (escape)
	($string-fold-left/list (lambda (knil . item*)
				  (cond ((apply pred item*)
					 => escape)
					(else
					 knil)))
	  #f str*))))

;;; --------------------------------------------------------------------

(case-define $string-find
  ((pred vec)
   ($string-find pred vec #f))
  ((pred vec default)
   (call/cc
       (lambda (escape)
	 ($string-fold-left/1 (lambda (knil item)
				(if (pred item)
				    (escape item)
				  knil))
	   default vec)))))

;;; --------------------------------------------------------------------

(case-define* string-find
  ((pred vec)
   ($string-find pred vec #f))
  ((pred vec default)
   (begin-checks
     (assert-argument-type (__who__) "procedure" procedure? pred 1)
     (assert-argument-type (__who__) "list"      string?    vec  2))
   ($string-find pred vec default)))

(define-string-searcher string-for-all
  $string-for-all/1
  $string-for-all/2
  $string-for-all/3
  $string-for-all/list)

(define-string-searcher string-exists
  $string-exists/1
  $string-exists/2
  $string-exists/3
  $string-exists/list)


;;;; copying

(: string-copy! (string fixnum string fixnum fixnum -> string))
(define* (string-copy! dst.vec dst.start src.vec src.start src.end)
  (begin-checks
    (assert-argument-type (__who__) "string"      string?      dst.vec     1)
    (assert-argument-type (__who__) "fixnum"      fixnum?      dst.start   2)
    (assert-argument-type (__who__) "string"      string?      src.vec     3)
    (assert-argument-type (__who__) "fixnum"      fixnum?      src.start   4)
    (assert-argument-type (__who__) "fixnum"      fixnum?      src.end     5)
    (unless (and (<= 0 dst.start)
		 (<= dst.start (string-length dst.vec)))
      (assertion-violation (__who__)
	"invalid start index for destination string" dst.vec dst.start))
    (unless (and (<= 0 src.start)
		 (<= src.start (string-length src.vec)))
      (assertion-violation (__who__)
	"invalid start index for source string" src.vec src.start))
    (unless (and (<= 0 src.end)
		 (<= src.end (string-length src.vec)))
      (assertion-violation (__who__)
	"invalid end index for source string" src.vec src.end))
    (unless (<= (- src.end src.start)
		(- (string-length dst.vec) dst.start))
      (assertion-violation (__who__)
	"invalid range in source string for selected range in destination string"
	dst.vec dst.start src.vec src.start src.end))
    #| end of BEGIN-CHECKS |# )
  ($string-copy! dst.vec dst.start src.vec src.start src.end))

(define ($string-copy! dst.vec dst.start src.vec src.start src.end)
  (do ((i dst.start (add1 i))
       (j src.start (add1 j)))
      ((= j src.end)
       dst.vec)
    (string-set! dst.vec i (string-ref src.vec j))))


;;;; misc

(define* (sorted-string-binary-search item< vec sought)
  ;;Return false or a non-negative fixnum representing the index at which SOUGHT is present in VEC.
  ;;
  ;;Adapted from (retrieved on Thu Jul 21, 2016):
  ;;
  ;;  <https://www.cs.bgu.ac.il/~elhadad/scheme/binary-search.html>
  ;;
  (begin-checks
    (assert-argument-type (__who__) "procedure"   procedure?   item<   1)
    (assert-argument-type (__who__) "string"      string?      vec     2))
  (define vec.len (string-length vec))
  (if (zero? vec.len)
      #f
    (let loop ((start 0)
	       (stop  (sub1 vec.len)))
      (if (< stop start)
	  #f
	(let* ((mid-point (fxshr (+ start stop) 1))
	       (mid-value (string-ref vec mid-point)))
	  (cond ((item< sought mid-value)
		 (loop start (sub1 mid-point)))
		((item< mid-value sought)
		 (loop (add1 mid-point) stop))
		(else mid-point)))))))


;;;; done

#| end of module |# )

;;; end of file

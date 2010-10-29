;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; Copyright (c) 2010, Simon David Pratt <me@simondavidpratt.com>          ;;;
;;;                                                                         ;;;
;;; Permission to use, copy, modify, and/or distribute this software        ;;;
;;; for any purpose with or without fee is hereby granted, provided         ;;;
;;; that the above copyright notice and this permission notice appear       ;;;
;;; in all copies.                                                          ;;;
;;;                                                                         ;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL           ;;;
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED           ;;;
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE        ;;;
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR                  ;;;
;;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM          ;;;
;;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,         ;;;
;;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN               ;;;
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; FILE:    augh.lisp                                                      ;;;
;;;                                                                         ;;;
;;; MODULE:  Another Upstart Generator of HTML                              ;;;
;;;                                                                         ;;;
;;; NOTES:   None.                                                          ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun include-html (path)
  "Simply reads a file into and returns a string."
  (with-open-file (file path)
    (let ((string (make-string (file-length file))))
      (read-sequence string file)
      string)))

(defun cat-string-list (s los)
  "Appends each string in the list of strings to the given string."
  (if (< 0 (length los))
      (cat-string-list (concatenate 'string s (car los)) (cdr los))
      s))

(defun build-html (tag-name inner-html &rest attributes)
  "Builds html from given tag-name, inner-html and attributes."
  (let ((html (concatenate 'string "<" tag-name)))
    (dolist (a attributes)
      (setf html (concatenate 'string html " " (car a) "=\"" (cadr a) "\"")))
    (concatenate 'string html ">" inner-html "</" tag-name ">")))

(defun html (&rest inner-html)
  "Builds and prints a valid html page."
  (format t
	  (concatenate 'string
		       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" "
		       "\"http://www.w3.org/TR/html4/strict.dtd\">~%"
		       (build-html "html" (cat-string-list "" inner-html)))))

(defun head (&rest inner-html)
  (build-html "head" (cat-string-list "" inner-html)))

(defun title (&rest inner-html)
  (build-html "title" (cat-string-list "" inner-html)))

(defun body (&rest inner-html)
  (build-html "body" (cat-string-list "" inner-html)))

(defun p (&rest inner-html)
  (build-html "p" (cat-string-list "" inner-html)))

(defun a (inner-html &key href)
  (build-html "a" inner-html (list "href" href)))

(defun br ()
  "<br>")

(defun h1 (&rest inner-html)
  (build-html "h1" (cat-string-list "" inner-html)))

(defun h2 (&rest inner-html)
  (build-html "h2" (cat-string-list "" inner-html)))

(defun h3 (&rest inner-html)
  (build-html "h3" (cat-string-list "" inner-html)))

(defun ul (&rest inner-html)
  (build-html "ul" (cat-string-list "" inner-html)))

(defun li (&rest inner-html)
  (build-html "li" (cat-string-list "" inner-html)))

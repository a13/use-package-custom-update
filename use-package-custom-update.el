;;; use-package-custom-update.el --- :custom-update keyword for use-package  -*- lexical-binding: t; -*-

;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (use-package "2.1"))
;; Author: Dmitry K.
;; Keywords: convenience, tools, extensions
;; Homepage: https://github.com/a13/use-package-custom-update

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The `:custom-update` keyword allows you customize variables
;; using functions instead of values
;;

;;; Code:

(require 'use-package)
(require 'subr-x)
(require 'seq)

(defun use-package-custom-update-union (old-value elements)
  "Add new ELEMENTS to OLD-VALUE list and return result.
Works similar to `cl-union', but keeps OLD-VALUE order."
  (if (listp old-value)
      (append old-value (cl-set-difference (if (listp elements)
                                               elements
                                             (list elements)) old-value
                                             :test #'equal))
    (use-package-error
     (concat old-value " is not a list"))))

(defcustom use-package-custom-update-default-updater #'use-package-custom-update-union
  "Function to update variable value."
  :type 'function
  :group 'use-package)

(defcustom use-package-custom-update-updater-use-symbol nil
  "If true - custom updater takes symbol as the first argument instead of its value."
  :type 'boolean
  :group 'use-package)


(defun use-package-normalize/:custom-update (_name keyword args)
  "Normalize use-package custom-update keyword."
  (use-package-as-one (symbol-name keyword) args
    #'(lambda (label arg)
        (unless (listp arg)
          (use-package-error
           (concat label " a (<symbol> <updater> [comment])"
                   " or list of these")))
        (if (use-package-non-nil-symbolp (car arg))
            (list arg)
          arg))))


(defun use-package-custom-update--dequote (x)
  "Remove car quote from X."
  (if (and (listp x)
           (eq 'quote (car x)))
      (cadr x)
    x))

(defun use-package-handler/:custom-update (name _keyword args rest state)
  "Generate use-package custom-update keyword code."
  (use-package-concat
   (mapcar
    #'(lambda (def)
        (let* ((variable (nth 0 def))
               (updater (nth 1 def))
               (comment (nth 2 def))
               (comment* (if (stringp comment)
                             comment
                           (format "Customized with use-package %s" name))))
          `(customize-set-variable (quote ,variable)
                                   ,(if (use-package-recognize-function updater)
                                        `(funcall ,updater ,(if use-package-custom-update-updater-use-symbol
                                                               `(quote ,variable)
                                                              `(symbol-value (quote ,variable))))
                                      `(funcall use-package-custom-update-default-updater
                                                (symbol-value (quote ,variable))
                                                (quote ,(use-package-custom-update--dequote updater))))
                                   ,comment*)))
    args)
   (use-package-process-keywords name rest state)))


(defun use-package-keywords-insert-after (kw new-kw)
  "Insert NEW-KW into `use-package-keywords' after KW."
  (unless (member new-kw use-package-keywords)
    (setq use-package-keywords
          (if-let ((pos (seq-position use-package-keywords kw)))
              (append (seq-take use-package-keywords (1+ pos))
                      (list new-kw)
                      (seq-drop use-package-keywords (1+ pos)))
            (append use-package-keywords (list new-kw))))))


(use-package-keywords-insert-after :custom :custom-update)

(provide 'use-package-custom-update)
;;; use-package-custom-update.el ends here

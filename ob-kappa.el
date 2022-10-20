;;; ob-kappa.el --- org-babel functions for Kappa evaluation

;; Copyright (C) 2022 William Waites

;; Author: William Waites
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'kappa-mode)

;; define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("kappa" . "ka"))

;; declare default header arguments for this language
(defvar org-babel-default-header-args:kappa
  '((:time . kappa-default-sim-time)
    (:events . kappa-default-sim-events)
    (:points . kappa-default-sim-points))

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:kappa' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:kappa (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-kappa nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%%var: %s %s"
                (car pair) (org-babel-kappa-var-to-kappa (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block. It writes the code block to a temporary file and calls
;; `kappa-run-sim' saving output to the file given in the `:file'
;; parameter or a temporary file if nil. It returns the file name
;; for the results.
;;
;; Other parameters understood are:
;; - `:time' simulation time limit (or zero)
;; - `:events' simulation event limit (or zero)
;; - `:points' simulation time per point
;; defaults are as per `kappa-default-sim-*' customisable
;; variables.
(defun org-babel-execute:kappa (body params)
  "Execute a block of Kappa code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Kappa source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         ; (result-params (assq :result-params processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         ;(result-type (cdr (assq :result-type processed-params)))
         ;; expand the body with `org-babel-expand-body:kappa'
	 (sim-time (eval (cdr (assq :time processed-params))))
	 (sim-events (eval (cdr (assq :events processed-params))))
	 (sim-points (eval (cdr (assq :points processed-params))))
         (full-body (org-babel-expand-body:kappa
                     body params processed-params))
	 (in-file (org-babel-temp-file "kappa-" ".ka"))
	 (out-file (or (cdr (assq :file processed-params))
		       (org-babel-temp-file "kappa-" ".csv"))))

    (with-temp-file in-file
      (insert full-body))
    (kappa-run-sim
     (org-babel-process-file-name in-file)
     (org-babel-process-file-name out-file)
     sim-time sim-events sim-points t)
    out-file
    ))

(defun org-babel-kappa-var-to-kappa (var)
  "Convert an elisp var into a string of kappa source code
specifying a var of the same value."
  (format "%S" var))

(provide 'ob-kappa)
;;; ob-kappa.el ends here

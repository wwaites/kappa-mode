;;; kappa.el -- major mode for Kappa models

;; Copyright (C) 2012 Sandro Stucki, Sebastian Jaramillo,
;;                    Ricardo Honorato-Zimmer
;;               2022 William Waites

;; Authors:
;;   Sandro Stucki <sandro.stucki@ed.ac.uk>
;;   Sebastian Jaramillo <sebajarar@gmail.com>
;;   Ricardo Honorato-Zimmer <rikardo.horo@gmail.com>
;;   William Waites <william.waites@strath.ac.uk>

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; `kappa-mode` is a GNU/Emacs major mode for editing files written in
;; the Kappa modeling language.

;; Customization group for Kappa mode.
(defgroup kappa nil
  "Kappa mode customization."
  :group 'languages)

;; Customization of fontification
(defface kappa-keyword-face
  '((t :inherit font-lock-preprocessor-face))
  "Face to use for highlighting Kappa keywords such as \"%var\"
or \"%init\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-command-face
  '((t :inherit font-lock-keyword-face))
  "Face to use for highlighting Kappa commands such as \"$ADD\"
or \"$STOP\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-rule-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting the Kappa rule operators \"@\"
and \"->\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-math-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting Kappa operators such as \"+\",
\"&&\" or \";\" in algebraic, logic and perturbation expressions
in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-interface-symbol-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting the Kappa agent interface symbols
such as \"!\" or \"~\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting built-in functions such as
\"[sin]\" or \"[mod]\" in Kappa expressions in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-constant-face
  '((t :inherit font-lock-constant-face))
  "Face to use for highlighting numerical and logical constants
such as \"[pi]\" or \"[true]\" in Kappa expressions in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-agent-name-face
  '((t :inherit font-lock-function-name-face))
  "Face to use for highlighting Kappa agent names in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-site-name-face
  '((t nil))
  "Face to use for highlighting Kappa site names in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-link-label-face
  '((t :inherit font-lock-type-face))
  "Face to use for highlighting the labels of links between Kappa
sites in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-internal-state-face
  '((t :inherit font-lock-variable-name-face))
  "Face to use for highlighting the internal state names of Kappa
sites in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-token-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face to use for highlighting Kappa token names in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-string-face
  '((t :inherit font-lock-string-face))
  "Face to use for highlighting string literals such as Kappa
variables and file names in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-comment-face
  '((t :inherit font-lock-comment-face
       :overline t
       :bold t))
  "Face to use for comment lines in Font-Lock mode."
  :group 'faces
  :group 'kappa)

;; Buffer-local variable definitions from face definitions
(defvar kappa-keyword-face 'kappa-keyword-face
  "Face for highlighting Kappa keywords.")
(defvar kappa-command-face 'kappa-command-face
  "Face for highlighting Kappa commands.")
(defvar kappa-rule-operator-face 'kappa-rule-operator-face
  "Face for highlighting Kappa rule operators.")
(defvar kappa-math-operator-face 'kappa-math-operator-face
  "Face for highlighting Kappa math and perturbation operators.")
(defvar kappa-interface-symbol-face 'kappa-interface-symbol-face
  "Face for highlighting Kappa agent interface symbols.")
(defvar kappa-builtin-face 'kappa-builtin-face
  "Face for highlighting built-in functions in Kappa mode.")
(defvar kappa-constant-face 'kappa-constant-face
  "Face for highlighting constants in Kappa mode.")
(defvar kappa-agent-name-face 'kappa-agent-name-face
  "Face for highlighting Kappa agent names.")
(defvar kappa-site-name-face 'kappa-site-name-face
  "Face for highlighting Kappa site names.")
(defvar kappa-link-label-face 'kappa-link-label-face
  "Face for highlighting Kappa link labels.")
(defvar kappa-internal-state-face 'kappa-internal-state-face
  "Face for highlighting Kappa internal state names.")
(defvar kappa-token-name-face 'kappa-token-name-face
  "Face for highlighting Kappa token names.")
(defvar kappa-string-face 'kappa-string-face
  "Face for highlighting string literals in Kappa mode.")
(defvar kappa-comment-face 'kappa-comment-face
  "Face for comment lines in Kappa mode.")

(defvar kappa-declarations
  '("%agent:" "%def:" "%var:" "%plot:" "%obs:" "%init:"
    "%mod:" "%token:"))

(defvar kappa-keywords
  '("do" "set" "repeat" "until"))

(defvar kappa-commands
  '("$ADD" "$DEL" "$FLUX" "$PLOTENTRY" "$PRINT" "$PRINTF"
    "$SNAPSHOT" "$STOP" "$TRACK" "$UPDATE"))

(defvar kappa-builtins
  '("[cos]" "[exp]" "[int]" "[log]" "[max]" "[min]" "[mod]"
    "[not]" "[sin]" "[sqrt]" "[tan]"))

(defvar kappa-constants
  '("[E]" "[E+]" "[E-]" "[Emax]" "[T]" "[Tmax]" "[Tsim]"
    "[false]" "[p]" "[pi]" "[true]" "INF"))

;; I'd probably put in a default that you want, as opposed to nil
(defvar kappa-tab-width 4 "Width of a tab for MYDSL mode")

(defvar kappa-font-lock-defaults
  (let
      ;; Common lexical sub-expressions used in keywords
      ((alnum "[A-Za-z0-9_+-]+")          ;; Alpha-numeric IDs
       (id "[A-Za-z][A-Za-z0-9_+-]*")     ;; IDs/names as defined in
       ;; the Kappa spec
       (num "[0-9]+")                     ;; Integer numerals
       (ws "\\(?:\\s-\\|\\\\\n\\)*"))     ;; Whitespace
    `((
       ("//.*\n"                             . kappa-comment-face)
       ("/\\*.*\\*/"                         . kappa-comment-face)
       ;; String literals and file names
       ("\"\\(?:[^\"\n]\\|\\\\[\"\n]\\)+\""  . kappa-string-face)
       ;; Variable names
       ("'[^'\n]+'"                          . kappa-string-face)
       ( ,(regexp-opt kappa-declarations)    . kappa-keyword-face)
       ( ,(regexp-opt kappa-keywords)        . kappa-keyword-face)
       ( ,(regexp-opt kappa-builtins)        . kappa-builtin-face)
       ( ,(regexp-opt kappa-constants)       . kappa-constant-face)
       ( ,(concat "[^\\[]\\(" num "\\)")     . kappa-constant-face)
       ( ,(regexp-opt kappa-commands)        . kappa-command-face)
       ("\\[\\([_.]\\)\\]"                   . '(1 kappa-interface-symbol-face))
       ( ,(concat "\\[\\(" num "\\)\\]")     . '(1 kappa-link-label-face))
       ( ,(concat "\\(" id "\\)" ws "(")     . '(1 kappa-agent-name-face))
       ( ,(concat "\\(" id "\\)" ws "{")     . '(1 kappa-site-name-face))
       ( ,(concat "{\\(" ws id "\\)")        . '(1 kappa-internal-state-face))
       ( ,(concat "\\(" id "\\)" ws "}")     . '(1 kappa-internal-state-face))
       ( ,(concat "{\\(\\(id\\)" ws "\\)*}") . '(1 kappa-internal-state-face))
       ("&&\\|||\\|:="                       . kappa-math-operator-face)
       ("|\\|->\\|<-\\|<->\\|@"              . kappa-rule-operator-face)
       ("[+*/^<>=.;-]"                       . kappa-math-operator-face)
))))

(define-derived-mode kappa-mode fundamental-mode "Kappa"
  "A major mode for editing models for the KaSim Kappa Simulator"
  (setq font-lock-defaults kappa-font-lock-defaults)
  (when kappa-tab-width
    (setq tab-width kappa-tab-width))
  (setq comment-start "//")
  (setq comment-end ""))

;;;
;;; Running simulations
;;;

;; Customisation of simulation-related parameters
(defcustom kappa-sim-executable-path (executable-find "KaSim")
  "File system path to the Kappa simulator executable."
  :type 'file
  :group 'kappa)

(defcustom kappa-gnuplot-executable-path (executable-find "gnuplot")
  "File system path to the Gnuplot executable."
  :type 'file
  :group 'kappa)

(defcustom kappa-default-sim-time 200
  "Default simulation duration."
  :type 'number
  :group 'kappa)

(defcustom kappa-default-sim-events 0
  "Default number of events to produce per simulation."
  :type 'number
  :group 'kappa)

(defcustom kappa-default-sim-points 1
  "Default time between output points."
  :type 'number
  :group 'kappa)

;; Buffer-local variables to remember the values of the arguments of
;; previous invocation of `kappa-run-sim' and `kappa-plot-sim'.
(defvar kappa-prev-sim-output-file ""
  "Value of the `output' or `file-path' argument during the
previous invocation of `kappa-run-sim' or `kappa-plot-sim',
respectively.")
(defvar kappa-prev-sim-time kappa-default-sim-time
  "Value of the `time' argument during the previous invocation of
`kappa-run-sim'.")
(defvar kappa-prev-sim-events kappa-default-sim-events
  "Value of the `events' argument during the previous invocation
of `kappa-run-sim'.")
(defvar kappa-prev-sim-points kappa-default-sim-points
  "Value of the `points' argument during the previous invocation
of `kappa-run-sim'.")
(defvar kappa-prev-plot-columns "1:2"
  "Value of the `columns' argument during the previous invocation
of `kappa-plot-sim'.")
(defvar kappa-sim-buffer-counter 1
  "Counts the number of simulation buffers in Kappa major mode")

(defun kappa-get-abs-dirname (path)
  "Return the absolute directory name of PATH."
  (file-name-directory (file-truename path)))

(defun kappa-run-sim (input output time events points force)
  "Input:
  INPUT: File path to the Kappa model.
  OUTPUT: Path to the simulation output file.
  TIME: Time (integer). Default is the value of
        `kappa-default-sim-time'.
  EVENTS: Number of events (integer). Default is the value of
          `kappa-default-sim-events'.
  POINTS: Number of points (integer). Default is the value of
          `kappa-default-sim-points'.
  FORCE: Overwrite output file without confirmation if it exists.
         Default is nil.

Output: none.

Side Effects: Prints the shell command to be executed to
*Messages*, Creates *Simulation* buffer, and runs
`kappa-sim-executable-path' in shell with the appropriate
arguments.

Related customization variables: `kappa-sim-executable-path',
`kappa-default-sim-time', `kappa-default-sim-events',
`kappa-default-sim-points'.
"
  (interactive
    (list (expand-file-name
           (read-file-name
            "Input: " (file-truename buffer-file-name)
            (file-truename buffer-file-name) 'confirm))
          (expand-file-name
           (read-file-name
            "Output: " (file-truename kappa-prev-sim-output-file)
            (file-truename kappa-prev-sim-output-file)))
          (read-number "Time: " kappa-prev-sim-time)
          (read-number "Events: " kappa-prev-sim-events)
          (read-number "Points: " kappa-prev-sim-points)
	  nil))

  ;; Save parameters for later
  (setq kappa-prev-sim-output-file output)
  (setq kappa-prev-sim-time time)
  (setq kappa-prev-sim-events events)
  (setq kappa-prev-sim-points points)

  ;; Construct the command line and try running the simulator in a
  ;; separate buffer.
  (let ((args (append (list "-i" input
                            "-o" (file-name-nondirectory output)
                            "-d" (kappa-get-abs-dirname output))
                      (cond
                       ((> time 0)   (list "-u" "time" "-l"
                                           (number-to-string time)))
                       ((> events 0) (list "-u" "event" "-l"
                                           (number-to-string events))))
                      (when points
                        (list "-p" (number-to-string points)))))
        (buffer-name (concat "*Simulation (" (file-name-nondirectory input)
                             ") " (number-to-string kappa-sim-buffer-counter)
                             "*")))

    (when (file-exists-p output)
      (if (or force (y-or-n-p (concat "Output file '" output
				      "' exists. Would you like to delete it to run "
				      "the simulation?")))
          (delete-file output)
        (error "%s" (concat "Output file " output
                            " has not been overwritten"))))

    ;; Save the command to *Message* buffer and run the simulation
    (message "Running simulation process: %s %s"
             kappa-sim-executable-path (mapconcat 'identity args " "))
    (let ((out-buffer (get-buffer-create buffer-name)))
      (display-buffer out-buffer)
      (apply 'call-process
             (append (list kappa-sim-executable-path nil out-buffer t)
                     args)))

    (setq kappa-sim-buffer-counter (+ 1 kappa-sim-buffer-counter))
    (message (format (concat "Done! See " buffer-name
                             " buffer for details")))))

(provide 'kappa-mode)

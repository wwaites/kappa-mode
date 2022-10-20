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
;; the Kappa modeling language and running them in the simulator.
;; The mode knows enough about Kappa syntax to do some basic
;; fontification but does currently not do indentation or proper
;; slashification.

;; There are numerous font face customization variables and a
;; convenience functions for running simulations and plotting
;; results. For more flexible plotting of results, see `ob-kappa.el'
;; which integrates with `org-mode'.

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

;;; Local key map

(defvar kappa-mode-keymap (make-sparse-keymap)
  "Kappa major mode keymap.")
;; Add shortcuts for `kappa-run-sim' and `kappa-plot-sim'.
(define-key kappa-mode-keymap "\C-c\C-r" 'kappa-run-sim)
(define-key kappa-mode-keymap "\C-c\C-p" 'kappa-plot-sim)

(define-derived-mode kappa-mode fundamental-mode "Kappa"
  "A major mode for editing models for the KaSim Kappa Simulator"
  (setq font-lock-defaults kappa-font-lock-defaults)
  (when kappa-tab-width
    (setq tab-width kappa-tab-width))
  (setq comment-start "//")
  (setq comment-end "")
  (use-local-map kappa-mode-keymap))

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
;(defvar kappa-default-sim-time kappa-default-sim-time)

(defcustom kappa-default-sim-events 0
  "Default number of events to produce per simulation."
  :type 'number
  :group 'kappa)
;(defvar kappa-default-sim-events kappa-default-sim-events)

(defcustom kappa-default-sim-points 1
  "Default time between output points."
  :type 'number
  :group 'kappa)
;(defvar kappa-default-sim-points kappa-default-sim-points)

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

;;; Gnuplot related functions and variables.

;; Global variable holding Gnuplot processes object for plotting if
;; necessary.
(defvar kappa-gnuplot-process ""
  "Process object of a running Gnuplot process.

The default value is \"\", indicating that no Gnuplot process has
been started by the Kappa major mode yet.")

(defun kappa-get-gnuplot-process nil
  "Return the process object of the Gnuplot process associated
with the Kappa major mode.

This function requires the installation of Gnuplot.  You can find
it at

  * http://www.gnuplot.info/

Side Effects: If `kappa-gnuplot-process' does not correspond to a
process object (no running Gnuplot process is currently
associated with the Kappa major mode), starts an asynchronous
process using the command `kappa-gnuplot-executable-path' and
returns the corresponding process object.  If
`kappa-gnuplot-process' refers to an existing Gnuplot process
with a status other than `run', kills it, starts a new
asynchronous Gnuplot process and returns the new process object.
In either case, prints the command to be executed to *Messages*,
creates a *Kappa Gnuplot output* buffer (if none exists) and sets
`kappa-gnuplot-process' to the process object associated with the
newly started process.

Related variables: `kappa-gnuplot-executable-path',
`kappa-gnuplot-process'.
"
  (let ((status (process-status kappa-gnuplot-process)))
    (if (eq status 'run) kappa-gnuplot-process

      ;; Kill a potentially "hanging" process.
      (if (not (null status)) (delete-process kappa-gnuplot-process))

      ;; Start a new asynchronous Gnuplot process.
      (message "Running Gnuplot as '%s -p'" kappa-gnuplot-executable-path)
      (setq kappa-gnuplot-process
            (start-process "gnuplot"
                           (get-buffer-create "*Kappa Gnuplot output*")
                           kappa-gnuplot-executable-path "-p")))))


(defun kappa-plot-sim (&optional file-path columns output-image)
  "Simple function for plotting a Kappa simulation file.

  FILE-PATH is the full path to a file that can be read by
            Gnuplot.  The first row is expected to contain the
            headers for each column.

  COLUMNS is a string containing the columns to be plotted
          separated by space.  Default is \"1:2\" plotting the
          first column (time) against the second one.

By default the following options would be set in Gnuplot:
autoscale, xtic auto, ytic auto, key autotitle columnhead,
ylabel \"Number of Molecules\", xlabel \"Time\"

This function requires the installation of Gnuplot and optionally
gnuplot-mode.  You can find them at

  * http://www.gnuplot.info/
  * https://github.com/bruceravel/gnuplot-mode/

Side Effects: If gnuplot-mode is available, sends commands to
gnuplot-mode.  Otherwise, send commands to a dedicated Gnuplot
process associated with the Kappa major mode.
"
  (interactive
   (list (expand-file-name
          (read-file-name
           "Simulation output file: "
           (file-truename kappa-prev-sim-output-file)
           (file-truename kappa-prev-sim-output-file) 'confirm))
         (read-string "Columns separated by space: "
                      kappa-prev-plot-columns)
	 (expand-file-name
	  (read-file-name
	   "Output image file: "
	   (file-truename (concat (substring kappa-prev-sim-output-file 0 -3) "png"))
	   (file-truename (concat (substring kappa-prev-sim-output-file 0 -3) "png")) 'confirm))
	 ))

  ;; Save parameters for later
  (setq kappa-prev-sim-output-file file-path)
  (setq kappa-prev-plot-columns columns)

  (let ((gnuplot-commands
         (concat
	  "set terminal png\n"
	  "set output \"" output-image "\"\n"
	  "set autoscale\n"
          "set xtic auto\n"
          "set ytic auto\n"
          "set key autotitle columnhead\n"
          "set ylabel \"Count\"\n"
          "set xlabel \"Time\"\n"
          "set title \"" (file-name-nondirectory file-path) "\"\n"
          "set datafile separator \",\"\n"
          "plot "
          (mapconcat
           (lambda (n) (concat "\"" file-path "\" using " n " with lines"))
           (split-string columns) ", \\\n") "\n")))

    ;; Check if gnuplot-mode is available, otherwise just run Gnuplot
    ;; in a shell.
    (if (require 'gnuplot nil t)

        ;; Use gnuplot-mode
        (gnuplot-send-string-to-gnuplot gnuplot-commands nil)

      ;; Send the Gnuplot commands to the Gnuplot process associated
      ;; with the Kappa major mode.
      (process-send-string (kappa-get-gnuplot-process) gnuplot-commands)))
  (find-file output-image))

(provide 'kappa-mode)

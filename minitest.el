;;; minitest.el --- An Emacs mode for ruby minitest files

;; Copyright © 2013-2014 Arthur Nogueira Neves

;; Author: Arthur Neves
;; URL: https://github.com/arthurnn/minitest-emacs
;; Version: 0.8.0
;; Package-Requires: ((dash "1.0.0"))

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'dash)
(require 'ansi-color)

(defcustom minitest-keymap-prefix (kbd "C-c ,")
  "Minitest keymap prefix."
  :group 'minitest
  :type 'string)

(defcustom minitest-use-zeus-when-possible t
  "When t and .zeus.sock is present, run specs with 'zeus'."
  :type 'boolean
  :group 'minitest)

(defcustom minitest-use-bundler t
  "minitest mode should use bundler?"
  :type 'boolean
  :group 'minitest)

(defcustom minitest-default-env nil
  "Default env vars for minitest"
  :type 'string
  :group 'minitest)

(defcustom minitest-use-spring nil
  "Use spring over zues and the default runner"
  :type 'boolean
  :group 'minitest)

(defun minitest-buffer-name (file-or-dir)
  (concat "*Minitest " file-or-dir "*"))

(defun minitest-test-command ()
   (cond (minitest-use-spring '("spring" "testunit"))
        ((minitest-zeus-p) "test")
        (t '("ruby" "-Ilib:test:spec"))))

(defun minitest-project-root ()
  "Retrieve the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (or (->> '("Rakefile" "Gemfile")
        (--map (locate-dominating-file default-directory it))
        (-remove #'null)
        (car))
      (error "You're not into a project")))

(defun minitest-zeus-p ()
  (and minitest-use-zeus-when-possible
       (file-exists-p (concat (minitest-project-root) ".zeus.sock"))))

(define-derived-mode minitest-compilation-mode compilation-mode ""
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(defun colorize-compilation-buffer ()
  (read-only-mode 1)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode -1))

(defvar minitest--last-command nil
  "Variable to store the last command running.")

(defun minitest--run-command (command &optional file-name)
  (if (fboundp 'rvm-activate-corresponding-ruby)
      (rvm-activate-corresponding-ruby))

  (let ((default-directory (minitest-project-root))
        (compilation-scroll-output t)
        (actual-command (concat (or minitest-default-env "") " " command)))
    (setq minitest--last-command (list command file-name))
    (compilation-start
     actual-command
     'minitest-compilation-mode
     (lambda (arg) (minitest-buffer-name (or file-name ""))))))

(defun minitest--file-command (&optional post-command)
  "Run COMMAND on currently visited file."
  (let ((file-name (buffer-file-name (current-buffer)))
        (bundle '("bundle" "exec"))
        (command (minitest-test-command))
        (zeus-command (if (minitest-zeus-p) "zeus" nil)))
    (if file-name
        (minitest--run-command
         (mapconcat 'shell-quote-argument
                    (-flatten
                     (--remove (eq nil it)
                               (list bundle zeus-command command file-name post-command))) " ")
         file-name)
      (error "Buffer is not visiting a file"))))

(defun minitest--extract-str ()
  (save-excursion
    (save-restriction
      (widen)
      (end-of-line)
      (or (re-search-backward "test '\\([^\"]+?\\)'" nil t)
          (re-search-backward "test \"\\([^\"]+?\\)\"" nil t)
          (re-search-backward "def test_\\([_A-Za-z0-9]+\\)" nil t)))))

(defun minitest-verify-all ()
  "Run all tests."
  (interactive)
  (minitest--run-command "bundle exec rake"))

(defun minitest-verify ()
  "Run on current file."
  (interactive)
  (minitest--file-command))

(defun minitest-verify-single ()
  "Run on current file."
  (interactive)
  (if (minitest--extract-str)
      (let* ((str (match-string 1))
             (post_command (replace-regexp-in-string " " "_" str)))
        (minitest--file-command
         (format  "-ntest_%s" post_command)))
      (error "No test found. Make sure you are on a file that has `def test_foo` or `test \"foo\"`")))

(defun minitest-rerun ()
  "Run the last command"
  (interactive)
  (if minitest--last-command
      (apply #'minitest--run-command minitest--last-command)
    (error "There is no previous command to run")))

;;; Minor mode
(defvar minitest-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "a") 'minitest-verify-all)
      (define-key prefix-map (kbd "v") 'minitest-verify)
      (define-key prefix-map (kbd "s") 'minitest-verify-single)
      (define-key prefix-map (kbd "r") 'minitest-rerun)
      (define-key map minitest-keymap-prefix prefix-map))
    map)
  "Keymap for minitest-mode.")

;;;###autoload
(define-minor-mode minitest-mode
  "Minor mode for *_test (minitest) files"
  :lighter " Minitest"
  :keymap minitest-mode-map
  :group 'minitest
  (if minitest-mode
      (progn
	(when (boundp 'yas-extra-modes)
	  (if (fboundp 'yas-activate-extra-mode)
	      ;; Yasnippet 0.8.1+
	      (yas-activate-extra-mode 'minitest-mode)
	    (make-local-variable 'yas-extra-modes)
	    (add-to-list 'yas-extra-modes 'minitest-mode)
	    (yas--load-pending-jits)))))
  )

(defvar minitest-snippets-dir
  (let ((current-file-name (or load-file-name (buffer-file-name))))
    (expand-file-name "snippets" (file-name-directory current-file-name)))
  "The directory containing minitest snippets.")

(defun minitest-install-snippets ()
  "Add `minitest-snippets-dir' to `yas-snippet-dirs' and load\
 snippets from it."
  (let ((yasnippet-available (require 'yasnippet nil t)))
    (if yasnippet-available
        (progn
          (add-to-list 'yas-snippet-dirs minitest-snippets-dir t)
          (yas-load-directory minitest-snippets-dir)))))

(defconst minitest-test-file-name-re "\\(_\\|-\\)test\\.rb\\'"
  "The regex to identify test files.")

(defun minitest-test-file-p (file-name)
  "Returns true if the specified file name is a test."
  (numberp (string-match minitest-test-file-name-re file-name)))

(defun minitest-buffer-is-test-p ()
  "Return true if the current buffer is a test."
  (and (buffer-file-name)
       (minitest-test-file-p (buffer-file-name))))

;;;###autoload
(defun minitest-enable-appropriate-mode ()
  (if (minitest-buffer-is-test-p)
      (minitest-mode)))

;;;###autoload
(dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
  (add-hook hook 'minitest-enable-appropriate-mode))

(provide 'minitest)
;;; minitest.el ends here

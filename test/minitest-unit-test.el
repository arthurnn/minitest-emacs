;;; minitest-unit-test.el --- unit test suite

;;; Commentary:

;;; Code:

(require 'test-helper)

(ert-deftest test-minitest-buffer-name ()
  (let ((filename nil))
    (should (equal (minitest-buffer-name filename) "*Minitest *")))
  (let ((filename "foo.rb"))
    (should (equal (minitest-buffer-name filename) "*Minitest foo.rb*"))))

(ert-deftest test-minitest-zeus-p ()
  (let ((minitest-use-zeus-when-possible nil))
    (should (equal (minitest-zeus-p) nil)))
  (let ((minitest-use-zeus-when-possible t))
    (should (equal (minitest-zeus-p) nil))))

(ert-deftest test-minitest-use-bundler-nil ()
  (let ((minitest-use-bundler nil))
    (should (equal (minitest-bundler-command) nil))))

(ert-deftest test-minitest-use-bundler-nil-on-verify-all ()
  (let ((minitest-use-bundler nil))
    (with-mock
     (mock (minitest--run-command "rake"))
      (minitest-verify-all))))

(ert-deftest test-minitest-use-bundler-nil-on-verify-file ()
  (let ((minitest-use-bundler nil))
    (with-mock
     (stub file-relative-name => "foo.rb")
     (mock (minitest--run-command "ruby -Ilib\\:test\\:spec foo.rb" "foo.rb"))
      (minitest--file-command))))

(ert-deftest test-minitest-test-command ()
  (let ((minitest-use-spring t))
    (should (equal (minitest-test-command) '("spring" "rake" "test"))))
  (let ((minitest-use-spring nil))
    (should (equal (minitest-test-command) '("ruby" "-Ilib:test:spec")))))

(ert-deftest test-minitest-test-command-override ()
    (with-mock
     (stub minitest-test-command => '("bin/rails" "test"))
     (stub file-relative-name => "foo.rb")
     (mock (minitest--run-command "bundle exec bin/rails test foo.rb" "foo.rb"))
      (minitest--file-command)))

(ert-deftest test-minitest-test-extract-str-with-method ()
  (with-temp-buffer
    (insert "def test_hello\nend")
    (goto-char (point-max))
    (minitest--extract-str)
    (should (equal (match-string 2) "hello"))))

(ert-deftest test-minitest--post-command ()
  (defvar test-description "#method_name behavior of Module::Class")
  (defvar method-name "_method_name_behavior_of_Module__Class")
  (should (equal method-name (minitest--post-command "test" test-description)))
  (should (equal method-name (minitest--post-command "it" test-description))))

(ert-deftest test-minitest-test-extract-str-with-block ()
  (with-temp-buffer
    (insert "test \"hello\" do\nend")
    (goto-char (point-max))
    (minitest--extract-str)
    (should (equal (match-string 2) "hello")))
  (with-temp-buffer
    (insert "test \"foo\" do\nend\ntest \'bar\' do\nend")
    (goto-char (point-max))
    (minitest--extract-str)
    (should (equal (match-string 2) "bar")))
  (with-temp-buffer
    (insert "test \'foo\' do\nend\ntest \"bar\" do\nend")
    (goto-char (point-max))
    (minitest--extract-str)
    (should (equal (match-string 2) "bar"))))

;; TODO
;;(ert-deftest test-minitest-test-extract-str-with-block-and-method ()
;;  (with-temp-buffer
;;    (insert "test \"foo\" do\nend\ndef test_bar\nend")
;;    (goto-char (point-max))
;;    (minitest--extract-str)
;;    (should (equal (match-string 2) "bar"))))

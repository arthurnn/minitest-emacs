;;; minitest-unit-test.el --- unit test suite

;;; Commentary:

;;; Code:

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
    (should (equal (minitest-test-command) minitest-spring-command)))
  (let ((minitest-use-docker t)
        (minitest-docker-container "app")
        (minitest-use-rails t))
    (should (equal (minitest-test-command) '("docker-compose" "exec" "app" "bin/rails" "test"))))
  (let ((minitest-use-docker t)
        (minitest-docker-command '("docker-compose" "run"))
        (minitest-docker-container "test"))
    (should (equal (minitest-test-command) '("docker-compose" "run" "test" "ruby" "-Ilib:test:spec"))))
  (let ((minitest-use-spring nil))
    (should (equal (minitest-test-command) '("ruby" "-Ilib:test:spec"))))
  (let ((minitest-use-docker t) (minitest-docker-container "test"))
    (should (equal (minitest-test-command) '("docker-compose" "exec" "test" "ruby" "-Ilib:test:spec")))))

(ert-deftest test-minitest-test-command-override ()
    (with-mock
     (stub minitest-test-command => '("bin/rails" "test"))
     (stub file-relative-name => "foo.rb")
     (mock (minitest--run-command "bundle exec bin/rails test foo.rb" "foo.rb"))
      (minitest--file-command)))

(ert-deftest test-minitest--post-command ()
  (defvar test-name "#method_name behavior of Module::Class")
  (defvar method-name "_method_name_behavior_of_Module__Class")
  (should (equal method-name (minitest--post-command test-name)))
  (should (equal method-name (minitest--post-command test-name))))

(ert-deftest test-minitest--extract-test-name ()
  "Tests extracting the test name for verify single command."
  ;; standard minitest method
  (with-temp-buffer
    (insert "def test_hello\nend")
    (goto-char (point-max))
    (should (equal (minitest--extract-test-name) "hello")))
  ;; rails minitest style
  (with-temp-buffer
    (insert "test \"hello\" do\nend")
    (goto-char (point-max))
    (should (equal (minitest--extract-test-name) "hello")))
  ;; different quote styles in the same file
  (with-temp-buffer
    (insert "test \"foo\" do\nend\ntest \'bar\' do\nend")
    (goto-char (point-max))
    (should (equal (minitest--extract-test-name) "bar")))
  (with-temp-buffer
    (insert "test \'foo\' do\nend\ntest \"bar\" do\nend")
    (goto-char (point-max))
    (should (equal (minitest--extract-test-name) "bar")))
  ;; with different test styles in the same buffer.
  (with-temp-buffer
    (insert "test \"foo\" do\nend\n\ndef test_bar\nend\nit 'is rad'\nend\nit \"is cool\"")
    (goto-char (point-max))
    (should (equal (minitest--extract-test-name) "is cool"))))

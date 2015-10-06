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

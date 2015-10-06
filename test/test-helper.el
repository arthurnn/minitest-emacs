(require 'f)

(require 'el-mock)
(eval-when-compile
    (require 'cl)) ;; for el-mock

(defvar minitest-test/test-path
  (f-parent (f-this-file)))

(defvar minitest-test/root-path
  (f-parent minitest-test/test-path))

(unless (require 'ert nil 'no-error)
  (require 'ert (f-expand "minitest" minitest-test/test-path)))

(require 'minitest (f-expand "minitest" minitest-test/root-path))

(ql:quickload :should-test)

(in-package :asdfx)

(defparameter *dir* (uiop:pathname-directory-pathname *load-truename*))

(defun load-from (dir &rest dirs)
  (let ((pkgs (list-all-packages)))
    (unwind-protect
         (let ((asdf:*central-registry*
                 (cons (merge-pathnames (format nil "test/~A/" dir) *dir*)
                       (append (mapcar (lambda (d)
                                         (merge-pathnames
                                          (format nil "test/~A/~A/" dir d)
                                          *dir*))
                                       dirs)
                               asdf:*central-registry*))))
           (qlx:load-system-with-renamings "root"))
      (progn
        (dolist (sys '("root" "foo" "bar" "baz" "quux" "prem" "bhav"))
          (asdf:clear-system sys))
        (dolist (pkg (reverse (set-difference (list-all-packages) pkgs)))
          (delete-package pkg))))))

(deftest zero-test ()
  (should print-to *debug-io*
          "baz
quux
foo
prem
bhav
bar
root
" (load-from "zero")))

(deftest basic-test ()
  (should print-to *debug-io*
          "baz
prem 1
foo
prem 2
bhav
bar
root
" (load-from "basic" "prem1" "prem2")))

(deftest cross-test ()
  (should print-to *debug-io*
          "baz 2
prem 1
foo
prem 2
baz 1
bar
root
" (load-from "cross" "prem1" "prem2" "baz1" "baz2")))

(deftest subroot-test ()
  (should print-to *debug-io*
          "bhav
prem
foo 1
bhav
prem
foo 2
baz
bar
root
" (load-from "subroot" "foo1" "foo2")))

(deftest inter-test ()
  (should print-to *debug-io*
          "prem 2
baz 2
quux 2
foo
prem 1
baz 1
quux 1
bar
prem 3
bhav
root
" (load-from "inter" "prem1" "prem2" "prem3" "baz1" "baz2" "quux1" "quux2")))

(deftest subinter-test ()
  (should print-to *debug-io*
          "baz 1
prem 1
foo 1
baz 2
prem 2
foo 2
prem 3
bar
root
" (load-from "subinter" "prem1" "prem2" "prem3" "foo1" "foo2" "baz1" "baz2")))

;;; (c) 2017 Vsevolod Dyomkin <vseloved@gmail.com>

(defpackage #:asdfx
  (:use :cl #+dev :should-test)
  (:export #:load-system-with-renamings
           #:sys #:make-sys #:sys-name #:sys-version #:sys-parent))

(in-package #:asdfx)


(defstruct sys
  name version parent)

(defun load-system-with-renamings (sys)
  (asdf::with-asdf-cache ()
    (multiple-value-bind (deps reverse-load-order renamings)
        (traverse-dep-tree sys)
      (declare (ignore deps))
      (when (zerop (hash-table-count renamings))
        ;; short-circuit to normal load-system
        (return-from load-system-with-renamings (asdf:load-system sys)))
      (let ((already-loaded (make-hash-table :test 'equal))
            (dep-packages (make-hash-table))
            conflicting-systems)
        ;; gather all systems with confflicting versions
        (maphash (lambda (sys deps)
                   (declare (ignore sys))
                   (dolist (dep deps)
                     (pushnew (sys-name dep) conflicting-systems
                              :test 'string-equal)))
                 renamings)
        ;; load dependencies one by one in topological sort order
        ;; renaming packages when necessary
        ;; and caching the results
        (dolist (dep (reverse reverse-load-order))
          (let* ((name (sys-name dep))
                 (conflict (and (member name conflicting-systems
                                        :test 'string-equal)
                                (sys-version dep))))
            (when (or conflict
                      (not (gethash name already-loaded)))
              (let ((known-packages (list-all-packages))
                    (pending-renamings (gethash dep renamings)))
                (if conflict
                    (load-system dep)
                    (load-components (asdf:find-system name)))
                ;; record packages newly defined after load of the current system
                (setf (gethash dep dep-packages)
                      (set-difference (list-all-packages) known-packages))
                ;; it's safe to rename pending packages now
                (dolist (d pending-renamings)
                  (let ((package-suffix (format nil "~:@(~A-~A-~A~)"
                                                (sys-version d)
                                                (sys-name dep)
                                                (gensym))))
                    (dolist (package (gethash d dep-packages))
                      (rename-package package (format nil "~A-~A"
                                                      (package-name package)
                                                      package-suffix)
                                      (mapcar (lambda (nickname)
                                                (format nil "~A-~A"
                                                        nickname
                                                        package-suffix))
                                              (package-nicknames package))))))))
            (unless conflict (setf (gethash name already-loaded) t))))))))

(defun traverse-dep-tree (sys &optional parent)
  (let ((cur (make-sys
              :name (if (listp sys)
                        (second sys)
                        sys)
              :version (when (listp sys) (third sys))
              :parent parent))
        (renamings (make-hash-table))
        ordered-deps
        known-deps
        merged-deps)
    (dolist (s (list-deps sys))
      (multiple-value-bind (deps load-order new-renamings)
          (traverse-dep-tree s cur)
        (dolist (d (rest deps))
          (setf (sys-parent d) (first deps)))
        (maphash (lambda (k v)
                   (setf (gethash k renamings) v))
                 new-renamings)
        (dolist (deps2 (reverse known-deps))
          (let ((conflicts (find-conflicts deps deps2)))
            (when conflicts
              (dolist (conflict conflicts)
                (dolist (d conflict)
                  (setf (gethash (sys-parent d) renamings)
                        (cons d (gethash (sys-parent d) renamings))))))))
        (push deps known-deps)
        (setf ordered-deps (cons load-order ordered-deps))))
    ;; if there are conflicting deps at top level
    ;; reorder deps so as to load conflicting paths first
    ;; (to ensure renaming happens in advance
    (let ((toplevel-renamings (gethash cur renamings)))
      (when toplevel-renamings
        (setf ordered-deps
              (sort ordered-deps (lambda (x y)
                                   (declare (ignore x))
                                   (not (find (first y) toplevel-renamings)))))))
    (values (cons cur
                  (when known-deps
                    (mapcar (lambda (sys)
                              (let ((merged-sys (find (sys-name sys) merged-deps
                                                      :test 'string-equal
                                                      :key 'car)))
                                (if merged-sys
                                    (cdr merged-sys)
                                    sys)))
                            (reduce (lambda (sys1 sys2)
                                      (union
                                       sys1 sys2
                                       :test
                                       (lambda (sys1 sys2)
                                         (when (string-equal (sys-name sys1)
                                                             (sys-name sys2))
                                           (push (cons (sys-name sys1)
                                                       (if (version> sys1 sys2)
                                                           sys1
                                                           sys2))
                                                 merged-deps)
                                           t))))
                                    known-deps))))
            (cons cur (reduce 'append ordered-deps))
            renamings)))

(defun find-conflicts (deps1 deps2)
  (let ((deps1 (sort (copy-seq deps1) 'string< :key 'sys-name))
        (deps2 (sort (copy-seq deps2) 'string< :key 'sys-name))
        rez)
    (do ((dep1 (pop deps1))
         (dep2 (pop deps2)))
        ((not (and dep1 dep2)))
      (cond ((string-equal (sys-name dep1) (sys-name dep2))
             (when (and (sys-version dep1)
                        (sys-version dep2)
                        (not (string= (sys-version dep1)
                                      (sys-version dep2))))
               (push (list dep1 dep2) rez))
             (setf dep1 (pop deps1)
                   dep2 (pop deps2)))
            ((string< (sys-name dep1) (sys-name dep2))
             (setf dep1 (pop deps1)))
            (t
             (setf dep2 (pop deps2)))))
    rez))


;;; ASDF workarounds

(defparameter *loading-with-renamings* nil)

(defun sysdef-exhaustive-central-registry-search (system)
  (let ((name (asdf:primary-system-name system))
        rez)
    (dolist (dir asdf:*central-registry*)
      (let ((defaults (eval dir)))
        (when (and defaults
                   (uiop:directory-pathname-p defaults))
          (let ((file (asdf::probe-asd name defaults
                                       :truename asdf:*resolve-symlinks*)))
            (when file
              (push file rez))))))
    (reverse rez)))

(defun find-system (dep)
  (unless (typep dep 'sys)
    (setf dep (typecase dep
                (list (make-sys :name (second dep) :version (third dep)))
                (t (make-sys :name dep)))))
  (let* ((name (sys-name dep))
         (sysname (asdf:coerce-name name))
         (version (sys-version dep))
         (asdf:*system-definition-search-functions*
           (cons 'sysdef-exhaustive-central-registry-search
                 asdf:*system-definition-search-functions*)))
    (dolist (asd (reduce 'append
                         (mapcar (lambda (fn)
                                   (let ((rez (funcall fn sysname)))
                                     (if (listp rez) rez (list rez))))
                                 asdf:*system-definition-search-functions*)))
      (when (version-satisfies name version asd)
        (asdf:load-asd asd)
        (return-from find-system (cdr (asdf:system-registered-p name)))))))

(defun load-system (dep)
  (let ((system (find-system dep)))
    (if system
        (load-components system)
        (error 'asdf:missing-component :requires (format nil "~A v.~A"
                                                         (sys-name dep)
                                                         (sys-version dep))))))

(defmethod asdf:component-depends-on :around ((o asdf:prepare-op)
                                              (s asdf:system))
  (unless *loading-with-renamings*
    (call-next-method)))

(defun load-components (sys)
  (let ((*loading-with-renamings* t))
    (dolist (c (asdf:module-components sys))
      (asdf:operate 'asdf:load-op c)))
  t)

(defun list-deps (sys)
  (let ((system (find-system sys)))
    (when system
      (asdf:system-depends-on system))))

(defun version> (sys1 sys2)
  (if (sys-version sys1)
      (if (sys-version sys2)
          (not (asdf::version< (sys-version sys1) (sys-version sys2)))
          t)
      nil))

(defun version-satisfies (sysname target-version asd)
  (with-open-file (in asd)
    (let ((*read-eval* nil))
      (loop :for form := (read in nil) :while form :do
        (when (and (string-equal "defsystem" (symbol-name (first form)))
                   (string-equal sysname (symbol-name (second form))))
          (return (string>= (asdf/parse-defsystem::normalize-version
                             (getf form :version) :pathname asd)
                            target-version)))))))


(require 'key-leap)
(require 'ert)
(require 'el-mock)

(defun buffer-with-n-lines (n)
  (let ((buffer (generate-new-buffer "temp")))
    (with-current-buffer buffer
      (dotimes (_ (- n 1))
        (newline)))
    buffer))

(defvar test-buffer (buffer-with-n-lines 100))

(defvar current-char-index 0)
(defvar input-key "")

(defun stub-input-char-source ()
  (let ((current-char (string-to-char (substring input-key current-char-index))))
    (setq current-char-index (+ 1 current-char-index))
    current-char))

(defun assert-jumps-to-line (line-num key)
  (setq current-char-index 0)
  (setq input-key key)
  (with-mock
    (stub window-top => 0)
    (key-leap--read-keys 'stub-input-char-source)
    (key-leap--leap-to-current-key)
    (should (= line-num (line-number-at-pos (point))))))

(ert-deftest does-thing ()
  "It jumps to the expected lines"
  (key-leap-set-key-chars '(?h ?g ?t ?c)
                          '(?a ?o ?e ?u)
                          '(?h ?t ?n ?s))
  (with-current-buffer test-buffer
    (assert-jumps-to-line 1 "hah")
    (assert-jumps-to-line 2 "hat")
    (assert-jumps-to-line 13 "huh")))

(defun test-key-leap ()
  (interactive)
  (load-file (buffer-file-name))
  (ert t))

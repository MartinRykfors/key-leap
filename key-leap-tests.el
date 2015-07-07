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
  "Function for stubbing user input when entering keys. 
Will enter the key as specified in the var input-key"
  (let ((current-char (string-to-char (substring input-key current-char-index))))
    (setq current-char-index (+ 1 current-char-index))
    current-char))

(defun assert-jumps-to-line (line-num key)
  (with-current-buffer test-buffer
    (key-leap-mode 1)
    (setq current-char-index 0)
    (setq input-key key)
    (with-mock
     (stub window-top => 0)
     (key-leap--read-keys 'stub-input-char-source)
     (key-leap--leap-to-current-key)
     (should (= line-num (line-number-at-pos (point)))))))

(setq key-leap-key-strings '("htns"
                             "aoeu"
                             "htns"))

(ert-deftest leaping-tests ()
  "It leaps to the expected lines"
  (assert-jumps-to-line 1 "hah")
  (assert-jumps-to-line 2 "hat")
  (assert-jumps-to-line 13 "huh"))

(defun assert-has-margin-text-on-line (line-num expected-key-string)
  (with-current-buffer test-buffer
    (key-leap-mode 1)
    (with-mock
     (stub window-top => 0)
     (goto-line line-num)
     ;; horrible hack follows:
     ;; I am able to get the 'before-string property of the overlay,
     ;; which looks like
     ;; #(\" \" 0 1 (display ((margin left-margin) #(\"hat\" 0 3 ...))))
     ;; I can't find any way of actually inspecting the property
     ;; closer to get the margin string, so I had to settle for this
     ;; solution instead: convert the entire property to a string and
     ;; use string-match to find the key-string
     (let* ((ol (first (overlays-at (point))))
            (prop (overlay-get ol 'before-string))
            (prop-string (prin1-to-string prop)))
       (string-match ".*\"\\([a-z]+\\)\".*" prop-string)
       (should (string= expected-key-string (downcase (match-string 1 prop-string))))))))

;; This will fail unless leaping-tests have been run in the test-buffer
;; It seems that it will not create overlays in the test buffers unless we first simulate leaping in that buffer, so we have an outcome dependency between these two tests.
(ert-deftest overlay-tests ()
  "It inserts the expected overlays at each line"
  (assert-has-margin-text-on-line 1 "hah")
  (assert-has-margin-text-on-line 2 "hat")
  (assert-has-margin-text-on-line 16 "hus")
  (assert-has-margin-text-on-line 17 "tah"))

(ert-deftest overlay-at-last-line ()
  "It has an overline also in the last line of a buffer"
  (with-current-buffer (buffer-with-n-lines 3)
    (with-mock
      (stub window-top => 0)
      (key-leap-mode 1)
      ;; for some reason, I need to simulate leaping in a buffer before it gets populated with overlays
      (setq current-char-index 0)
      (setq input-key "hah")
      (key-leap--read-keys 'stub-input-char-source)
      (key-leap--leap-to-current-key)
      (should (= 1 (length (overlays-in (point-max) (point-max))))))))

(defun test-key-leap ()
  (interactive)
  (load-file (buffer-file-name))
  (ert t))

;;Main File - Created by Alexandre Lavoie 2018

(loop
;; Read File
(format t "~%File Path: ")
(setf path (read))
(defun get-file (filename) (with-open-file (stream filename) (loop for line = (read-line stream nil) while line collect line)))
(setf file (get-file path))

;;Global Variable
(setf *c* 1) ;; Counter
(setf *h* 0) ;; Halt Flag
(setf *r* (make-array '(256))) ;; RAM
(setf *s* 0) ;; Sum Register
(setf *y* 0) ;; Carry Flag
(setf *z* 0) ;; Zero Flag
(setf *max-bit* 1024) ;; Carry Trigger

;;Functions
(defmacro ADD (register address) `(progn (setf *s* (parse-integer (nth (- ,address 1) file))) (setf *s* (+ ,register *s*)) (setf ,register *s*) (if (>= *s* *max-bit*) (setf *y* 1) (setf *y* 0)))) ;;Add ROM to Register and Set Register
(defmacro ADR (register address) `(progn (setf *s* (aref *r* ,address)) (setf *s* (+ ,register *s*)) (setf ,register *s*) (if (>= *s* *max-bit*) (setf *y* 1) (setf *y* 0)))) ;;Add RAM to Register and Set Register 
(defmacro ADI (register v) `(progn (setf *s* ,v) (setf *s* (+ ,register *s*)) (setf ,register *s*) (if (>= *s* *max-bit*) (setf *y* 1) (setf *y* 0)))) ;;Add Value to Register and Set Register
(defun HLT () (setf *h* 1)) ;;Halt Program
(defun JC (address) (if (= *y* 1) (progn (setf *c* (- address 1)) (setf *y* 0)))) ;;Jump If Carry Flag On
(defun JRC (offset) (if (= *y* 1) (progn (setf *c* (- (+ *c* offset) 1)) (setf *y* 0)))) ;;Jump Relative If Carry Flag On
(defmacro JMP (address &optional register) `(progn (if (not (eq ,register nil)) (setf ,register (+ *c* 1))) (setf *c* (- ,address 1)))) ;;Jump to Address
(defmacro JMR (offset &optional register) `(progn (if (not (eq ,register nil)) (setf ,register (+ *c* 1))) (setf *c* (- (+ *c* ,offset) 1)))) ;;Jump Relative to Current Address
(defmacro JR (register) `(setf *c* (- ,register 1))) ;; Jump to Value in Register/ Jump Return
(defun JZ (address) (if (= *z* 1) (progn (setf *c* (- address 1)) (setf *z* 0)))) ;;Jump If Zero Flag On
(defun JRZ (offset) (if (= *z* 1) (progn (setf *c* (- (+ *c* offset) 1)) (setf *z* 0)))) ;;Jump Relative If Zero Flag On
(defmacro LDI (register v) `(setf ,register ,v)) ;;Load Value to Register
(defmacro LDR (register address) `(setf ,register (parse-integer (nth ,address file)))) ;;Load from ROM to Register
(defun MX (v) (setf *max-bit* v));;Sets Maximum to Trigger Carry Flag
(defmacro MOV (register1 register2) `(setf ,register2 ,register1)) ;;Move Register 1 to Register 2
(defmacro OUT (register) `(print ,register)) ;;Output Register
(defmacro RTR (register address) `(setf ,register (aref *r* ,address))) ;;Read RAM to Register
(defun RUT () (print *r*)) ;;Print RAM
(defmacro RRR (registeroutput registeraddress) `(setf ,registeroutput (aref *r* ,registeraddress))) ;;Read RAM at Address RegisterAddress to RegisterOutput
(defmacro SBR (register address) `(progn (setf *s* (aref *r* ,address)) (setf *s* (- ,register *s*)) (setf ,register *s*) (if (<= *s* 0) (setf *z* 1) (setf *z* 0)))) ;;Substract RAM to Register and Set Register
(defmacro SBI (register v) `(progn (setf *s* ,v) (setf *s* (- ,register *s*)) (setf ,register *s*) (if (<= *s* 0) (setf *z* 1) (setf *z* 0)))) ;;Substract Value to Register and Set Register
(defmacro SRR (registervalue registeraddress) `(setf (aref *r* ,registeraddress) ,registervalue)) ;;Stores RegisterValue at RegisterAddress to RAM
(defmacro STR (register address) `(setf (aref *r* ,address) ,register)) ;;Stores Register to RAM
(defmacro SVR (v address) `(setf (aref *r* ,address) ,v)) ;;Stores Value to RAM
(defmacro SUB (register address) `(progn (setf *s* (parse-integer (nth (- ,address 1) file))) (setf *s* (- ,register *s*)) (setf ,register *s*) (if (<= *s* 0) (setf *z* 1) (setf *z* 0)))) ;;Substract ROM to Register and Set Register

;; Run Loop
(loop (eval (read-from-string (nth (- *c* 1) file))) (setf *c* (+ *c* 1)) (when (= *h* 1) (return "")))

(format t "~%"))
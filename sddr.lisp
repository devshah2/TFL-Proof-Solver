(defvar *rules* nil)

(defun ask (query *rules*)
  (error-on-bad-rules *rules*)
  (mapcar #'(lambda (blist)
              (instantiate query blist))
    (prove query)))

(defun rap (rules)
  (error-on-bad-rules rules)
  (flet ((prompt () (format t "~&? ") (read))) 
    (do ((x (prompt) (prompt))) 
        ((atom x)) 
      (print (ask x rules)))))

(defun error-on-bad-rules (rules)
  (let ((bad-rules (remove-if 'rule-p rules)))
    (when bad-rules
      (error "Found rules not in (<- consequent ...) form:~{~%  ~S~}"
             bad-rules))))

(defun rule-p (rule)
  (and (consp rule) (eql (car rule) '<-) (cdr rule)))


;;; The key data structure is a list of binding lists (blists).
;;; NIL means failure.
;;; (NIL) is success, namely a list of an empty binding list,
;;; as when (A B) matches (A B).

;;; (PROVE query rules) -> blists
;;;   Returns the appended list of successful binding lists for
;;;   all rules that prove the query. Rule variables
;;;   are renamed before use.
;;;
;;; (APPLY-RULE rule query) -> blists
;;;   Returns the binding lists produced when using rule to
;;;   prove query.  
;;;
;;; (BACKCHAIN queries blists) -> list of binding lists
;;;   Returns a list of the binding lists found by proving
;;;   all the queries with each blist.
;;;
;;; (RENAME-VARIABLES pat) -> pat
;;;   Returns a copy of pat with all variables replaced by
;;;   variables with new names.
;;;
;;; (INSTANTIATE pat blist) -> pat
;;;   Returns a copy of pat with all variables replaced by
;;;   their var-values.

(defun prove (query &optional (blists (list nil)))
  (if (eql (car query) 'not)
      (prove-not (cadr query) blists)
    (loop for rule in *rules* append
          (apply-rule (rename-variables rule) query blists))))

(defun prove-not (query blists)
  (remove-if (lambda (blist) (prove query (list blist)))
             blists))

(defun apply-rule (rule query blists)
  (backchain (cddr rule)
             (unify (cadr rule) query blists)))

(defun backchain (queries blists)
  (if (null queries)
      blists
    (loop for blist in blists append
          (backchain (cdr queries)
                     (prove (instantiate (car queries) blist) 
                            (list blist))))))


;;; Renaming variables and replacing them with their bindings
;;; are very similar operations
(defun rename-variables (pat)
  (replace-vars pat (lambda (var) (declare (ignore var)) (gensym "?"))))

(defun instantiate (pat blist)
  (replace-vars pat (lambda (var) (var-value var blist))))

(defun replace-vars (pat replacer)
  (sublis (mapcar (lambda (var) 
                    (cons var (funcall replacer var)))
                  (form-vars pat))
          pat))

;;; Get all vars in form with no duplicates
(defun form-vars (form &optional vars)
  (cond ((var-p form) (adjoin form vars))
        ((atom form) vars)
        (t
         (form-vars (cdr form)
                    (form-vars (car form) vars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unifier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (UNIFY pat1 pat2 [list of blists]) -> blists
;;;   Returns a list of the binding lists that unify pat1 and pat2.
;;;
;;; (VAR-UNIFY var pat blists) => blists
;;;   Returns a list of the binding lists that unify a variable
;;;   with a pattern.
;;;
;;; (BIND-VAR var pat blist) -> blists
;;;   Returns NIL if var can't be unified with pat, otherwise
;;;   returns a list of blist extended with bindings for var,
;;;   if needed.
;;;   - A variable unifies with itself with no new binding
;;;   - A bound variable unifies with pat if its binding
;;;     unifies with pat
;;;   - An unbound variable unifies with anything except a
;;;     functional term containing the variable
;;;
;;; (VAR-VALUE var blist) -> value
;;;   Returns the recursively determined binding of a variable:
;;;    - var, if var has no binding or is bound to itself
;;;    - else the instantiated binding of var

(defun unify (pat1 pat2 &optional (blists (list nil)))
  (cond ((null blists) nil)
        ((var-p pat1) (var-unify pat1 pat2 blists))
        ((var-p pat2) (var-unify pat2 pat1 blists))
        ((atom pat1) (and (eql pat1 pat2) blists))
        ((atom pat2) nil)
        (t (unify (cdr pat1) (cdr pat2)
                  (unify (car pat1) (car pat2) blists)))))

(defun var-unify (var pat blists)
  (loop for blist in blists append
        (bind-var var pat blist)))

(defun bind-var (var pat blist)
  (cond ((var-equalp var pat blist)
         (list blist))
        ((var-bound-p var blist)
         (unify (var-binding var blist) pat (list blist)))
        ((contained-in-p var pat blist) nil)
        (t (list (cons (list var pat) blist)))))

(defun contained-in-p (var pat blist)
  (if (var-p pat)
      (or (eql var pat)
          (contained-in-p var (var-binding pat blist) blist))
      (and (consp pat)
           (or (contained-in-p var (car pat) blist)
               (contained-in-p var (cdr pat) blist)))))

(defun var-equalp (var1 var2 blist)
  (and (var-p var2)
       (or (eql var1 var2)
           (var-equalp var1 (var-binding var2 blist) blist))))

(defun var-p (x)
 (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun var-binding (var blist) (cadr (assoc var blist)))
(defun var-bound-p (var blist) (not (null (assoc var blist))))

(defun var-value (var blist)
  (if (var-bound-p var blist)
      (let ((value (var-binding var blist)))
        (if (eql var value)
            var
          (instantiate value blist)))
    var))

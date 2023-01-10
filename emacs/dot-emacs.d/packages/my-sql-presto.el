(require 'sql)

(sql-add-product 'presto "Presto"
                 '(:free-software t))

(defcustom my-sql-presto-program "presto"
  "Command to start Presto cli."
  :type 'file
  :group 'SQL)

(sql-set-product-feature 'presto
                         :sqli-program 'my-sql-presto-program)
(sql-set-product-feature 'presto
                         :prompt-regexp "^presto.*> ")

(defcustom my-sql-presto-login-params '(user server database)
  "Login parameters needed to connect to Presto."
  :type 'sql-login-params
  :group 'SQL)

(sql-set-product-feature 'presto
                         :sqli-login 'my-sql-presto-login-params)

(defcustom my-sql-presto-options nil
  "List of additional options for `sql-presto-program'."
  :type '(repeat string)
  :group 'SQL)

(sql-set-product-feature 'presto
                         :sqli-options 'my-sql-presto-options)

(defun my-sql-comint-presto (product options &optional buf-name)
  "Connect to Presto in a comint buffer."
  (let ((params
         (append
          (if (not (string= "" sql-user))
              (list "--user" sql-user))
          (if (not (string= "" sql-server))
              (list "--server" sql-server))
          (if (not (string= "" sql-database))
              (let ((catalog-schema (split-string "hive.seo_prod.test" "\\." t)))
                (append
                 (list "--catalog" (car catalog-schema))
                 (if (not (null (cdr catalog-schema)))
                     (list "--schema" (car (cdr catalog-schema)))))))
          options)))
    (setenv "PRESTO_PAGER" "")
    (sql-comint product params buf-name)))

(sql-set-product-feature 'presto
                         :sqli-comint-func 'my-sql-comint-presto)

(defun my-sql-presto (&optional buffer)
  "Run Presto cli as an inferior process."
  (interactive "P")
  (sql-product-interactive 'presto buffer))

(provide 'my-sql-presto)

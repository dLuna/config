;; Erlang-templates

(defun erlang-skel-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 2)))
    (concat (make-string percent ?%) 
	    " "
	    (make-string (- 71 percent) ?-) 
	    "\n")))

(defun erlang-skel-double-separator (&optional percent)
  "Return a comment separator."
  (let ((percent (or percent 2)))
    (concat (make-string percent ?%) 
	    " "
	    (make-string (- 71 percent) ?=) 
	    "\n")))

(setq erlang-skel-export
  '(& "-export([])." n))

(setq erlang-skel-lgpl-comment
  '("%% This library is free software; you can redistribute it and/or modify" n
    "%% it under the terms of the GNU Lesser General Public License as" n
    "%% published by the Free Software Foundation; either version 2 of the" n
    "%% License, or (at your option) any later version." n
    "%%" n
    "%% This library is distributed in the hope that it will be useful, but" n
    "%% WITHOUT ANY WARRANTY; without even the implied warranty of" n
    "%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU" n
    "%% Lesser General Public License for more details." n
    "%%" n
    "%% You should have received a copy of the GNU Lesser General Public" n
    "%% License along with this library; if not, write to the Free Software" n
    "%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307" n
    "%% USA" n))

(setq erlang-skel-cvs-comment
  '(& "%% $Id" ":$ " n))

(setq erlang-skel-copyright-comment
  '(& "%% @copyright " (format-time-string "%Y ") (user-full-name) n))
;;  '(& "%% @copyright " (format-time-string "%Y ") "SiftLogic LLC" n))

(setq erlang-skel-mail-address
      '"daniel@lunas.se")

(setq erlang-skel-author-comment
  '(& "%% @author " (user-full-name) " <" erlang-skel-mail-address ">" n))

(setq erlang-skel-file-comment
  '(& "%% File: " (file-name-nondirectory buffer-file-name) n))

(setq erlang-skel-indent-level
      '(& "%% -*- erlang-indent-level: 2 -*-" n))

(setq erlang-skel-small-header
  '(o (erlang-skel-include erlang-skel-module)
      "-author('" (user-full-name) " <" erlang-skel-mail-address ">')." n
      (erlang-skel-include erlang-skel-export)
      n))

(setq erlang-skel-normal-header
  '(o (erlang-skel-include ;;erlang-skel-indent-level
			   ;;erlang-skel-cvs-comment
			   )
      ;; n
      (erlang-skel-include erlang-skel-author-comment
			   erlang-skel-copyright-comment)
      "%% @doc " p n 
      (erlang-skel-include erlang-skel-small-header)))

(setq erlang-skel-large-header
  '(o (erlang-skel-include ;;erlang-skel-indent-level
			   ;;erlang-skel-cvs-comment
			   )
      (erlang-skel-double-separator)
      (erlang-skel-include erlang-skel-lgpl-comment
			   erlang-skel-author-comment
			   erlang-skel-copyright-comment)
      "%% " n
      "%% @doc " p n
      "%% " n
      n
      (erlang-skel-include erlang-skel-small-header)))

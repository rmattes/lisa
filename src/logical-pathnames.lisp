;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)
;;; Copyright (C) 2016 Ralf Mattes <rm@mh-freiburg.de>
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: logical-pathnames.lisp
;;; Description: Installs logical pathname translations for lisa's source files

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((base (asdf:system-source-directory :lisa))
         (lisa-root-pathname (make-pathname :directory
                                            (pathname-directory base)
                                            :host (pathname-host base)
                                            :device (pathname-device base))))
    (flet ((make-lisa-path (relative-path)
             (concatenate 'string (namestring lisa-root-pathname)
                          relative-path)))

      (setf (logical-pathname-translations "lisa")
            `(("src;**;" ,(make-lisa-path "src/**/"))
              ("lib;**;*.*" ,(make-lisa-path "lib/**/"))
              ("config;*.*" ,(make-lisa-path "config/"))
              ("debugger;*.*" ,(make-lisa-path "src/debugger/"))
              ("contrib;**;" ,(make-lisa-path "contrib/**/")))))))

#!/usr/bin/env -S sbcl --script
(print 1)
(require :uiop)
(format t "hello ~a!~&" (uiop:getenv "USER"))
(loop repeat 1000000 do (format t "Hello, World!%"))

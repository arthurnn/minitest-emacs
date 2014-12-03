# minitest.el

[![Build Status](https://travis-ci.org/arthurnn/minitest-emacs.svg?branch=master)](https://travis-ci.org/arthurnn/minitest-emacs)

A minitest mode for emacs

# Installation

## Manual

Just drop `minitest.el`. somewhere in your `load-path`. And:
```lisp
(add-to-list 'load-path "~/emacs.d/vendor")
(require 'minitest)
```

## Using [Marmalade](http://marmalade-repo.org/) or [MELPA](http://melpa.milkbox.net/)

```
M-x package-install minitest
```

# Usage

Command                                         | Description                                             | Mode binding
------------------------------------------------|---------------------------------------------------------|--------------------
<kbd>M-x minitest-verify</kbd>                  | Runs minitest test on the current file                  | `C-c , v`
<kbd>M-x minitest-verify-single</kbd>           | Runs minitest test on the selected line                 | `C-c , s`
<kbd>M-x minitest-rerun</kbd>                   | Runs the last test again                                | `C-c , r`
<kbd>M-x minitest-verify-all</kbd>              | Runs all the tests in the project                       | `C-c , a`


To enable minitest mode on ruby files:

```lisp
(add-hook 'ruby-mode-hook 'minitest-mode)
```

## Snippets

To add snippets to yasnippet:
```lisp
(eval-after-load 'minitest-mode
  '(minitest-install-snippets))
```

## License

The MIT License (MIT)

Copyright (c) 2013 Arthur Nogueira Neves

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

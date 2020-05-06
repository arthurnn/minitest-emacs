# minitest.el
### A minitest mode for emacs

[![Build Status](https://travis-ci.org/arthurnn/minitest-emacs.svg?branch=master)](https://travis-ci.org/arthurnn/minitest-emacs)
[![MELPA](https://melpa.org/packages/minitest-badge.svg)](https://melpa.org/#/minitest)

# Usage

Command                                         | Description                                             | Mode binding
------------------------------------------------|---------------------------------------------------------|--------------------
<kbd>M-x minitest-verify</kbd>                  | Runs minitest test on the current file                  | `C-c , v`
<kbd>M-x minitest-verify-single</kbd>           | Runs minitest test on the selected line                 | `C-c , s`
<kbd>M-x minitest-rerun</kbd>                   | Runs the last test again                                | `C-c , r`
<kbd>M-x minitest-verify-all</kbd>              | Runs all the tests in the project                       | `C-c , a`

# Installation

## Manual

### Emacs

Just drop `minitest.el`. somewhere in your `load-path`. And:
```lisp
(add-to-list 'load-path "~/emacs.d/vendor")
(require 'minitest)
```

### Spacemacs

The Spacemacs Ruby layer supports minitest-emacs as one of its configurable test runners.
To set the default Ruby test runner to use this package, edit the `ruby` layer entry in
the `dotspacemacs-configuration-layers` list defined in your `.spacemacs` file:

```lisp
dotspacemacs-configuration-layers
'(
  (ruby :variables
        ruby-test-runner 'minitest)
)

```

See the Spacemacs Ruby layer documentation for more info.

## Using [Marmalade](http://marmalade-repo.org/) or [MELPA](http://melpa.milkbox.net/)

```
M-x package-install minitest
```

# Configuration

To enable minitest mode on ruby files, simply add to your dotfile:

```lisp
(add-hook 'ruby-mode-hook 'minitest-mode)
```

In case you are using Spacemacs, add the above line to `dotspacemacs/user-config` which is towards the end
of your `.spacemacs` dotfile:

```lisp
(defun dotspacemacs/user-config ()
	(add-hook 'ruby-mode-hook 'minitest-mode))
```
If you have another hook already in here make sure to add this hook within its own set of parenthesis so that
there is only one hook per parenthesis.

## Rails

If you are working on a Rails project, you can set `minitest-use-rails` to true in order to use the `bin/rails test`
command when running examples.

## Docker

You can run tests inside a Docker container by setting `minitest-use-docker` to
true and `minitest-docker-container` to the name of the container. By default this
will use the command `docker-compose exec` to run the minitest command, which assumes
you already have the specified container running. To customize the command, edit the
`minitest-docker-command` variable.

## Snippets

If you have yasnippet installed, you can load the snippets:
```lisp
(eval-after-load 'minitest
  '(minitest-install-snippets))
```

## Scroll Position

The `compilation-scroll-output` variable, when set to `t`, scrolls to the bottom
of the test output. When set to `nil`, the test output buffer stays scrolled to
the top.

Before version 0.10.0, this variable was set to `t` before running tests, but
after version 0.10.0 it is not set explicitly anymore. In order to get back the
old behavior, just set `compilation-scroll-output` to `t` yourself.

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

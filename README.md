# minitest.el
### A minitest mode for emacs

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

Just drop `minitest.el`. somewhere in your `load-path`. And:

```lisp
(require 'minitest)
```

## Using [MELPA](https://melpa.org)

```
M-x package-install minitest
```

# Configuration

To enable minitest mode on ruby files, simply add this hook to your dotfile:

```lisp
(add-hook 'ruby-mode-hook 'minitest-mode)
```

## Rails

The Rails test runner is invoked using `bin/rails test` instead of the default minitest command.
The `minitest-use-rails` custom variable can be set to `t` to invoke the Rails runner.

## Docker

You can run tests inside a Docker container by setting `minitest-use-docker` to
`t` and `minitest-docker-container` to the name of the container. By default this
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

If you'd like to automatically scroll to the bottom of the test report, set the
`compilation-scroll-output` variable to `t`.

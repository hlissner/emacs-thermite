![Release tag](https://img.shields.io/github/tag/hlissner/emacs-doom-themes.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/doom-themes-badge.svg?style=flat-square)](http://melpa.org/#/doom-themes)
[![Build Status](https://travis-ci.org/hlissner/emacs-doom-themes.svg?branch=master&style=flat-square)](https://travis-ci.org/hlissner/emacs-doom-themes)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

# doom-themer

doom-themer is a library for writing themes for GNU Emacs! Why? Because writing
themes for Emacs is such a hassle and it shouldn't have to be.

This library provides:

+ Linear inheritance of face specs and the palette (variables)
+ Two comprehensive base themes called `doom-base-dark` and `doom-base-light`
  with coverage for 100+ packages and counting
+ An elisp-based import/export mechanism for getting your doom themes out into
  your favorite apps and back. No external dependencies required!
+ A secondary library of helper functions for fetching information about the
  current theme, like color information.
+ Commands for generating color schemes based on a variety of sources: pywal,
  images, etc.

# Table of Contents

- [Features](#features)
- [Install](#install)
- [Common Issues](#common-issues)
- [Contribute](#contribute)

## Install

This package ~~is~~ will be available on MELPA:

`M-x package-install RET doom-themer`

However, themer authors should place it in the `Package-Requires` header of your
package, and require `doom-themer-base` (if you are extending from it):

```emacs-lisp
;;; my-theme.el -*- lexical-binding: t; -*-
;;
;; Author: John Doe <http://github/...>
;; Created: July 1, 2040
;; Modified: July 1, 2040
;; Version: 1.0.0
;; Keywords: themes faces customization
;; Homepage: https://github.com/.../my-theme
;; Package-Requires: ((emacs "25.1") (doom-themer "3.0"))
;;
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'doom-themer-base))
  
(define-doom-theme my-theme
  ...)
```

## Writing a theme

...


## Exporting a theme to ...

...


## Importing a theme from ...

...

## Generating themes

...

# Contributing

...

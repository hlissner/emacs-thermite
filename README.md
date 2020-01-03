![Release tag](https://img.shields.io/github/tag/hlissner/emacs-doom-themer.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/doom-themer-badge.svg?style=flat-square)](http://melpa.org/#/doom-themer)
[![Build Status](https://travis-ci.org/hlissner/emacs-doom-themer.svg?branch=master&style=flat-square)](https://travis-ci.org/hlissner/emacs-doom-themer)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

> **This project is a work-in-progress and should not be used yet.**

# doom-themer

doom-themer makes writing themes for GNU Emacs easier. How? With theme
inheritance, abstracting colors and styles behind logical groups, and sane
defaults for over 100 packages and counting.

# TODO

+ [X] Linear inheritance of face specs and theme palettes/logical groups.
+ [ ] Two comprehensive base themes called `doom-base-dark` and
  `doom-base-light` with coverage for 100+ packages and counting.
+ [X] A secondary library of helper functions for fetching information about the
  current theme, like color information.
+ [ ] An elisp-based import/export mechanism for getting your doom themes out
  into your favorite apps and back. No external dependencies required!
+ [ ] Commands for generating color schemes based on a variety of sources:
  pywal, images, etc.

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

# Similar/related projects

- doom-themes
- autothemr

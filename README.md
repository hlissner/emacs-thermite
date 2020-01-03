![Release tag](https://img.shields.io/github/tag/hlissner/emacs-doom-themer.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/doom-themer-badge.svg?style=flat-square)](http://melpa.org/#/doom-themer)
[![Build Status](https://travis-ci.org/hlissner/emacs-doom-themer.svg?branch=master&style=flat-square)](https://travis-ci.org/hlissner/emacs-doom-themer)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

> **This project is a work-in-progress and should not be used yet.**

# doom-themer

doom-themer makes writing themes for GNU Emacs easier.

**How?** With theme inheritance, abstracting colors and styles behind logical
groups, and sane base themes that cover over 100 packages and counting that
folks can extend for their own themes. And users can extend *those* after that.

**Why?** Because writing themes for Emacs is a hassle. Every theme needs to
reinvent the wheel to cover all them packages. 

I wrote this to replace the "API" [doom-themes] currently uses, which is so
super slow that it [murders the byte-compiler][poor-byte-compiler]. I knew
diddly squat about E-to-the-lisp when I wrote it; now that I have 99 more bullet
holes in my shoes, I can make that 100th count!

**TODO:**

+ [X] Linear inheritance of face specs and theme palettes/logical groups.
+ [ ] Two comprehensive base themes called `doom-base-dark` and
  `doom-base-light` with coverage for 100+ packages and counting.
+ [ ] A comprehensive and stable list of logical groups for our two base themes
  to define, to categorize all UI/syntax/semantic elements under.
+ [X] A secondary library of helper functions for fetching information about the
  current theme, like color information.
+ [ ] An elisp-based import/export mechanism for getting your doom themes out
  into your favorite apps and back. No external dependencies required!
+ [ ] Commands for generating color schemes based on a variety of sources:
  pywal, images, etc.


**Table of Contents**

- [Install](#install)
- [Documentation](#documentation)
    - [Writing a theme](#writing-a-theme)
    - [Exporting a theme to ...](#exporting-a-theme-to-)
    - [Importing a theme from ...](#importing-a-theme-from-)
    - [Generating themes](#generating-themes)
- [Contributing](#contributing)
- [Similar/related projects](#similarrelated-projects)

# Install

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
  
(define-doom-theme (my-theme :extend doom-base-dark)
  ...)
```


# Documentation

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


[doom-themes]: https://github.com/hlissner/emacs-doom-themes
[poor-byte-compiler]: https://github.com/hlissner/emacs-doom-themes/issues/314

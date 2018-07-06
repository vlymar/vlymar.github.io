---
title: 'Sharpen Your Tools'
layout: post
date: 2018-07-05
category: 'posts'
---

In this one I explore Emacs self-documentation and customization features to make my IDE just a bit better.

<!--more-->

I've been meaning to do this for a while but have been stuck in the "tar pit of immediacy." [^1] I do a lot of Rails development in Spacemacs and I love how easy it is to run specs and view the output. With a spec buffer open `, t b` opens a compilation buffer with the test output. The problem is the output is not very space efficient. It runs rspec with the `-b` flag, which includes the full error backtrace. A simple failure in a single spec can easily have ~50 lines of traceback, most of it useless.

I'm going to document my exploration of the ruby test runner code, and my attempt to make the output more appropriate for Spacemacs.

First things first - lets find an entry point. One of my favorite features of Spacemacs is command discoverability and self-documentation. I've already learned that `, t b`[^2] runs all the tests in the buffer...But what code does it execute? Lets try that again, slower.

![](Screen%20Shot%202018-07-02%20at%205.15.10%20PM.png)

In this screencap I've typed `, t` and paused. Now we see the functions bound to the keyboard shortcuts. `b` is bound to `ruby-test-run`, so thats our entry point.

If you have a function name, the next step is to use Emac's help built in functionality. With Spacemacs you can get to it with `SPC h d f`[^3]. Now type in the function you want help with and hit enter.

![](Screen%20Shot%202018-07-02%20at%205.20.21%20PM.png)

Here we see the file the function is defined in, the function's signature, and some documentation.

Get your cursor over `ruby-test-mode.el` and hit enter. You'll instantly be taken to the definition location of the function :)

```lisp
;;;###autoload
(defun ruby-test-run ()
  "Run the current buffer's file as specification or unit test."
  (interactive)
  (let ((filename (ruby-test-find-file)))
	(if filename
		(ruby-test-with-ruby-directory filename
		 (ruby-test-run-command (ruby-test-command filename)))
	  (message ruby-test-not-found-message))))
```

This code handles a bit of plumbing but since we're laser focused on just the command being run we can hone in on `(ruby-test-run-command (ruby-test-command filename))`.  `ruby-test-command` looks like promising, so lets check it out. Move your cursor over it and run `gd` to jump to its definition.

```lisp
(defun ruby-test-command (filename &optional line-number)
  "Return the command to run a unit test or a specification depending on the FILENAME and LINE-NUMBER."
  (cond ((ruby-test-spec-p filename)
		 (ruby-test-spec-command filename line-number))
		((ruby-test-p filename)
		 (ruby-test-test-command filename line-number))
		(t (message "File is not a known ruby test file"))))
```

This looks like a conditional that determines whether it should dispatch to RSpec or Test::Unit. We're only interested in RSpec in the moment so lets jump into `ruby-test-spec-command`.

```lisp
(defun ruby-test-spec-command (filename &optional line-number)
  "Return command to run spec in FILENAME at LINE-NUMBER."
  (let ((command
		 (cond ((file-exists-p ".zeus.sock") "zeus rspec")
			   ((file-exists-p "bin/rspec") "bin/rspec")
			   (t "bundle exec rspec")))
		(options ruby-test-rspec-options)
		(filename (if line-number
					  (format "%s:%s" filename line-number)
					filename)))
	(format "%s %s %s" command (mapconcat 'identity options " ") filename)))
```

We're close now. I can feel it. Lets look at `ruby-test-rspec-option`:

```lisp
(defcustom ruby-test-rspec-options
  '("-b")
  "Pass extra command line options to RSpec when running specs."
  :initialize 'custom-initialize-default
  :type '(list)
  :group 'ruby-test)
```

Boom! This passes `-b` into the command, filling our test buffer with dozens of lines of trash. But what's `defcustom`? As an Emacs newbie I'd never seen that before so I googled it and landed on it's [docs](https://www.gnu.org/software/emacs/manual/html_node/eintr/defcustom.html). Turns out Emacs has a  GUI config system called the "customization interface." `defcustom` is a function for adding config parameters along with their default values and setter functions.

All thats left now is to override the value for this variable in your `.spacemacs`. (`SPC f e d` takes you right to it). Jump to your `dotspacemacs/user-config` section and add the following:

```lisp
;; disable rspec backtrace (-b) option
(setq ruby-test-rspec-options '())
```

Now reload your config (`SPC f e R`) and we're done!

### Things that didn't make it into this post
1. A discussion of the tradeoffs between `set`, `custom-set-variables`, `customize-set-variable`. This is a bit of a rabbit hole. In short - if the variable in question uses a setter function, `setq` won't cut it. You'd know because `defcustom` would have a `:set` argument.
2. Instead of reading the code you can use a debugger. Try `M-x debug-on-entry`, then enter the function name you want to trace.

**Reference**
[^1]: https://blog.aaronbieber.com/2014/02/12/sharpening-your-blades.html
[^2]: `,` is a shortcut for `SPC m`, where your major mode keybinds are.
[^3]: `SPC h` is a good starting point for exploration if you're new to Spacemacs.
[https://confreaks.tv/videos/cascadiaruby2011-the-unix-chainsaw](https://confreaks.tv/videos/cascadiaruby2011-the-unix-chainsaw)

---
title: 'Sharpen Your Tools'
published: 2018-07-11
---

In this one I explore Emacs's self-documentation and customization features to make my IDE just a bit better.

<!--more-->

Spacemacs is a great Rails IDE. With a spec file open `, t b` opens the compilation buffer and runs the tests. The problem is the output is not very space efficient. It runs rspec with the `-b` flag, which includes the full error backtrace. A simple failure in a single spec can easily have ~50 lines of traceback, most of it useless.

![annoying-trace]({{ "/assets/img/posts/sharpen-your-tools/annoying-trace.png" | absolute_url }})

I'm going to document my exploration of the Emacs ruby test runner code, and my attempt to make the output more appropriate for Spacemacs.

First things first - lets find an entry point. One of my favorite features of Spacemacs is command discoverability and self-documentation. I already know that `, t b`[^1] runs all the tests in the buffer...But what code does it execute? Lets try that again, slower.

![command-discovery]({{ "/assets/img/posts/sharpen-your-tools/command-discovery.png" | absolute_url }})

Type `, t` and pause. Now we see that if we added `b` we'd execute `ruby-test-run`, so thats our entry point.

If you have a function name, the next step is to use Emacs's built in help functionality. With Spacemacs you can get to it with `SPC h d f`[^2]. Now type in the function you want help with and hit enter.

![ruby-test-run-docs]({{ "/assets/img/posts/sharpen-your-tools/ruby-test-run-docs.png" | absolute_url }})

Here we see the file the function is defined in, the function's signature, and some documentation.

Get your cursor over `ruby-test-mode.el` and hit enter. You'll instantly be taken to the definition of the function :)

```common_lisp
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

This code handles a bit of plumbing but since we're laser focused on just the command being run we can hone in on `(ruby-test-run-command (ruby-test-command filename))`.  `ruby-test-command` looks like promising, so lets check it out. Move your cursor over it and type `gd` to jump to its definition.

```common_lisp
(defun ruby-test-command (filename &optional line-number)
  "Return the command to run a unit test or a specification depending on the FILENAME and LINE-NUMBER."
  (cond ((ruby-test-spec-p filename)
		 (ruby-test-spec-command filename line-number))
		((ruby-test-p filename)
		 (ruby-test-test-command filename line-number))
		(t (message "File is not a known ruby test file"))))
```

This looks like a conditional that determines whether it should dispatch to RSpec or Test::Unit. We're only interested in RSpec in the moment so lets jump into `ruby-test-spec-command`.

```common_lisp
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

We're close now. I can feel it. Lets look at the definition of `ruby-test-rspec-options`:

```common_lisp
(defcustom ruby-test-rspec-options
  '("-b")
  "Pass extra command line options to RSpec when running specs."
  :initialize 'custom-initialize-default
  :type '(list)
  :group 'ruby-test)
```

Boom! This passes `-b` into the command, filling our test buffer with dozens of lines of trash. But what's `defcustom`? As an Emacs newbie I'd never seen that before so I googled it and landed on its [docs](https://www.gnu.org/software/emacs/manual/html_node/eintr/defcustom.html). Turns out Emacs has a  GUI config system called the "customization interface." `defcustom` is a function for adding config parameters along with their default values and setter functions.

At this point you could use the customization GUI to modify the value for `ruby-test-rspec-options`. My preference is to stick with simple file based config.

All thats left now is to override the value for this variable in your `.spacemacs`. (`SPC f e d` takes you right to it). Jump to your `dotspacemacs/user-config` section and add the following:

```common_lisp
;; disable rspec backtrace (-b) option
(setq ruby-test-rspec-options '())
```

Now reload your config (`SPC f e R`) and we're done!

![rspec-no-backtrace]({{ "/assets/img/posts/sharpen-your-tools/rspec-no-backtrace.png" | absolute_url }})

### Things that didn't make it into this post
1. A discussion of the tradeoffs between `setq`, `custom-set-variables`, `customize-set-variable`. This is a bit of a rabbit hole. In short - if the variable in question uses a setter function, `setq` won't cut it. You'd know because `defcustom` would have a `:set` argument.
2. Instead of reading the code you could use a debugger. Try `M-x debug-on-entry`, then enter the function name you want to trace.

### Reference

[^1]: `,` is a shortcut for `SPC m`, where your major mode keybinds are.
[^2]: `SPC h` is a good starting point for exploration if you're new to Spacemacs.


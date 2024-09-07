---
title: "Some IRB Tricks"
published: 2018-07-01
---

The REPL is a programmer's best friend.

<!--more-->

### "`_`" evaluates to the REPL's last return value
You're patting yourself on the back for that slick one-liner you just got right on the first try, but then you realize...._you forgot to add one to it_. Quickly re-use the return value of the previous expression with the `_` keyword.

```ruby
> [1, 2, 3].reduce(:+)
=> 6

> _ + 1
=> 7
```

### `; nil` for your sanity
A common irb workflow is to iteratively build up a short program while saving intermediate variables along the way. Assignment (`=`) is an expression in ruby [^1], which means assignment returns the value that was assigned. If you're assigning a large object, you might be stuck for a while watching output scroll by. 

Rails example:
```ruby
> bobs = Users.where(name: "Bob")
=> < 30 seconds of ActiveRecord objects being printed to the console... >
```

My workaround is to append `; nil` to each line. The assignment still happens, but the REPL will quickly print `nil`, letting you keep working on your program. This is extra nice when you're building up an Active Record query because AR queries are lazily evaluated. When you add `; nil` after the AR query assignment, the query won't be executed until you print or use the results.

Example:
```ruby
> bobs = Users.where(name: "Bob"); nil
=> nil

> adult_bobs = bobs.where("age > ?", 30); nil
=> nil

> adult_bobs.count
=> 100
```

### Easy introspection
Ruby has the most intuitive and comprehensive standard library I've ever seen. I'll find myself wondering "Does X support operation Y?" and more often than not, it does (though I'm starting to forget where Ruby ends and ActiveSupport begins). Take advantage of the fact that everything is an object:

```ruby
> Array.methods
=> [:[], :try_convert, :new, :allocate, :superclass, :<=>, :module_exec, :class_exec, :<=, :>=, :==, :===, :include?, :included_modules, :ancestors, :name, :public_instance_methods, :instance_methods, :private_instance_methods, :protected_instance_methods, :const_get, :constants, :const_defined? 
    ...
```

`Object#methods` returns _all_ the methods for the object, including inherited ones. Here's a trick for pruning out the superclass methods:

```ruby
> Array.methods - Array.superclass.methods
=>[:[], :try_convert]
```

Usually the bulk of the methods I'm trying to filter out belong to `Object`, so I'll just subtract `Object.methods` instead of typing out `SomeKlass.superclass.methods`.

Note that the previous example only returns `Array`'s class methods. To get the instance methods:

```ruby
> arr_methods = Array.new.methods - Object.new.methods
=> [:transpose, :fill, :assoc, :rassoc, :uniq, :uniq!, :compact, :compact!, :to_h, :flatten, :flatten!, :shuffle!, :include?, :permutation, :combination, :sample, :repeated_combination, :shuffle, :product, :bsearch, :bsearch_index, :repeated_permutation, :shelljoin, :map!, :&, :*,
    ...
```

Sometime's I'll just `sort` and scan the methods. Other time's I have a sense of what I'm looking for and I'll `Array#grep`. Two things worth mentioning:
1. [Ruby-doc.org](https://ruby-doc.org/core-2.5.1/Array.html) is a fantastic source of documentation.
2. [Pry](http://pryrepl.org/) is a better way to introspect on Ruby classes.

### .irbrc goodies
If you find yourself requiring the same libraries over and over, or re-defining the same helper methods, throw them into an `.irbrc` file in your home directory. Here's a barebones one to start you off with some goodies:

```ruby
require 'irb/completion' # tab completion!
require 'pp' # pretty printing

IRB.conf[:AUTO_INDENT] = true
```

<br>

---

[^1]: Everything is an expression in Ruby.

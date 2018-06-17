---
layout: post
title: "Some Ruby Tricks"
date: 2018-05-16
---
> Then they come up to me and say, "I was surprised by this feature of the language, so therefore Ruby violates the principle of least surprise." Wait. Wait. The principle of least surprise is not for you only. The principle of least surprise means principle of least my surprise. And it means the principle of least surprise after you learn Ruby very well.

> Yukihiro Matsumoto [https://www.artima.com/intv/ruby4.html][1]

Over 4 years of working with Ruby I've learned a couple of tricks, conventions and footguns. Here are a few.

## IRB / Rails console
The REPL is a programmer's best friend.

### "`_`" evaluates to the REPL's last return value

```ruby
irb(main):> [1, 2, 3].reduce(:+)
=> 6

> _
=> 6

> _ + 1
=> 7

```


You're patting yourself on the back for that slick one-liner you just got right on your first try, but then you realize....\_you forgot to add one to it\_. Quickly re-use the return value of the previous expression with the `_` keyword.

### `; nil` for your sanity
A common irb workflow is to iteratively build up a short program, saving intermediate variables along the way. Assignment (`=`) is an expression in Ruby, meaning it returns the value that was assigned. If you're assigning a large object, you might be stuck for a while watching output scroll by. 

Rails example:
```ruby
> bobs = Users.where(name: "Bob")
=> < 30 seconds of ActiveRecord objects being printed to the console >
```

My workaround is to append `; nil` to each line. The assignment still happens, but the REPL will quickly print `nil`, letting you keep working on your program.

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
Ruby has the most intuitive and comprehensive standard library I've ever seen. I'll find myself wondering "Does X support operation Y?" and more often than not, it does (though I've been working with Rails for so long that sometimes the lines between ActiveSupport and the standard lib. are blurred). Take advantage of the fact that ruby has great introspection features and that everything is an object.

```ruby
> Array.methods
=> [:[], :try_convert, :new, :allocate, :superclass, :<=>, :module_exec, :class_exec, :<=, :>=, :==, :===, :include?, :included_modules, :ancestors, :name, :public_instance_methods, :instance_methods, :private_instance_methods, :protected_instance_methods, :const_get, :constants, :const_defined? 
    ...
```

`Object#methods` returns _all_ the methods for the object, including inherited ones, which can be overwhelming. Here's a trick for pruning out the superclass methods:

```ruby
> Array.methods - Array.superclass.methods
=>[:[], :try_convert]
```

Usually the bulk of the methods I'm trying to filter out belong to `Object`, so I'll just subtract `Object.methods` instead of typing out `SomeKlass.superclass.methods`.

Note that the previous above only returns `Array`'s class methods. To get the instance methods:

```ruby
> arr_methods = Array.new.methods - Object.new.methods
=> [:transpose, :fill, :assoc, :rassoc, :uniq, :uniq!, :compact, :compact!, :to_h, :flatten, :flatten!, :shuffle!, :include?, :permutation, :combination, :sample, :repeated_combination, :shuffle, :product, :bsearch, :bsearch_index, :repeated_permutation, :shelljoin, :map!, :&, :*,
    ...
```

Sometime's I'll just `sort` and scan the methods. Other time's I have a sense of what I'm looking for and I'll `Array#grep`. Two things worth mentioning:
1. [Ruby-doc.org][2] is a fantastic source of documentation.
2. [Pry][3] is a much more powerful way to introspect on Ruby classes (and much more).

## Patterns
Here are some patterns often found in Ruby projects.

### `||=`
Commonly used to assign a variable who's current value is nil. Takes advantage of the fact that `nil` evaluates as `false` in Ruby.

```ruby
foo = nil
foo ||= 1
foo ||= 2 # does not assign 2
foo == 1 #=> true
```

### `Symbol#to_proc`
One of the less immediately intuitive keywords in Ruby is `&:symbol`, which is a shorthand for `Symbol#to_proc` ([documentation][4]). 

```ruby
[1, 2, 3].map(&:to_s) #=> ["1", "2", "3"]
[1, 2, 3].select(&:even?) #=> [2]

# equivalent to the above:
[1, 2, 3].map { |i| i.to_s } #=> ["1", "2", "3"]
[1, 2, 3].select { |i| i.even? } #=> [2]
```

`Symbol#to_proc`  returns a `Proc` that accepts a single argument and then calls the method named by the symbol on that argument.

### Struct
`Person = Struct.new(:name, :birthdate)`
```ruby
class Person < Struct.new(:name, :birthdate)
  def birth_year
    @birthdate.year
  end
endel
```

### `&&` vs `and`, `||` vs `or`
When I was mentoring interns at Scribd, a common issue was use of `and` and `or` (particularly with developers with Python backgrounds).
TODO

### `class << self`
TODO

[1]:	https://www.artima.com/intv/ruby4.html
[2]:	https://ruby-doc.org/core-2.5.1/Array.html
[3]:	http://pryrepl.org/
[4]:	[https://ruby-doc.org/core-2.5.1/Symbol.html#method-i-to%5C_proc]
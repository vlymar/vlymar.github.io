---
title: 'Knowing Ruby Well'
layout: post
date: 2018-05-26
category: 'posts'
---
> Then they come up to me and say, "I was surprised by this feature of the language, so therefore Ruby violates the principle of least surprise." Wait. Wait. The principle of least surprise is not for you only. The principle of least surprise means principle of least my surprise. And it means the principle of least surprise after you learn Ruby very well.
>  - Yukihiro Matsumoto [[source](https://www.artima.com/intv/ruby4.html)]

Here's a grab bag of patterns I've picked up over 4 years of working with Ruby.

<!--more-->

---

### Or-Equals (`||=`)
Commonly used to conditionally assign a variable whose current value is nil. Takes advantage of the fact that `nil` evaluates as `false` in Ruby.

```ruby
foo = nil
foo ||= 1 #=> 1
foo ||= 2 #=> 1
foo == 1  #=> true
```

See also: `&&=`.

### Converting symbols to procs
One of the less intuitive patterns in Ruby is `&:symbol`, which is shorthand for `Symbol#to_proc` ([documentation](https://ruby-doc.org/core-2.5.1/Symbol.html#method-i-to%5C_proc)). 

```ruby
[1, 2, 3].map(&:to_s) #=> ["1", "2", "3"]
[1, 2, 3].select(&:even?) #=> [2]

# equivalent to the above:
[1, 2, 3].map { |i| i.to_s } #=> ["1", "2", "3"]
[1, 2, 3].select { |i| i.even? } #=> [2]
```

Prefixing an object with `&` calls `to_proc` on that object. `Symbol#to_proc` returns a `Proc` that accepts a single argument and then calls the method named by the symbol on that argument.

### Bootstrapping classes with `Struct`
```ruby
# Variant 1
Car = Struct.new(:make, :model)

rental = Car.new('Honda', 'CRV')
rental.make  #=> 'Honda'
rental.model #=> 'CRV'

# Variant 2
require 'date'

class Person < Struct.new(:name, :birthdate)
  def birth_year
    birthdate.year
  end
end

bob = Person.new('Bob', Date.new(1984, 01, 02))
bob.birth_year #=> 1984
```

Inheriting from a Struct can save you some of the boilerplate associated with reading and writing attributes ([documentation](https://ruby-doc.org/core-2.4.2/Struct.html)).

### Splats

Here's an example of the `*` (splat) operator used for array expansion:

```ruby
# Array expansion
a = [1, 2]
b = [0, *a] # => [0, 1, 2]

def foo(a, b)
  "#{a}, #{b}"
end

c = [1, 2]
foo(*c) #=> "1, 2]

# Double splat
def bar(a:, b:)
  "#{a}, #{b}"
end

d = { a: 1, b: 2 }
bar(**d) #=> "1, 2"
```

This is just a taste of how splats can be used. See [this great post](http://blog.honeybadger.io/ruby-splat-array-manipulation-destructuring/) for more.
 
### `&&` vs `and`, `||` vs `or`
A common pitfall for those with a Python background is misuse of the `and`, `or` keywords. At first glance they appear to have the same behavior as `&&` and `||`, but they actually have lower precedence which can lead to surprising behavior. Here's a demo:

```ruby
x = true && false #=> false
x #=> false

y = true and false #=> false
y #=> true
```

Note that `x` is false and `y` is true. Take a look at the [ruby operator precedence](https://ruby-doc.org/core-2.4.2/doc/syntax/precedence_rdoc.html) docs. The `or`, `and` operations are lower than `=`, which is lower than `||` and `&&`. If you're unfamiliar with programming language operator precedence think of [PEMDAS](https://en.wikipedia.org/wiki/Order_of_operations). When I get confused about precedence, I like to wrap operations in parenthesis so I can visualize the order of operations. Here's the above example with parenthesis added to show order of operations:

```ruby
# `&&` has a higher precedence than `=`, so its evaluated first
x = (true && false)

# `=` has a higher precedence than `end`, so its evaluated first
(y = true) and false
```

Why does Ruby have these super low precedence boolean operators? They're sometimes used for program _control flow_. Check out the [render and return Rails pattern](http://guides.rubyonrails.org/layouts_and_rendering.html#avoiding-double-render-errors).

### Eigenclasses and @@

```ruby
class Foo
  class << self
    def bar
      1
    end
  end
end

Foo.bar #=> 1
```

You probably know `<<` as the shovel operator, useful for inserting objects into arrays (`[1, 2] << 3`). The `class << self` notation and the jargon associated with it (eigenclass, singleton class, metaclass) can be pretty obscure. A good starting point is to know that this syntax lets you implement class methods. My above example can be rewritten as the slightly more familiar:

```ruby
class Foo
  def self.bar
    1
  end
end

Foo.bar #=> 1
```

There's a lot to discuss on this subject but I want to focus on a very practical question: when do Rubyists use one form over the other? The key for me is `@@`. You've probably seen these used for "class variables."

Here's an abstract example demonstrating the pitfalls of using `@@` for class variables:

```ruby
class ParentClass
  def self.set_count(count)
    @@count = count
  end
  
  def self.count
    @@count
  end
end

ParentClass.set_count(3)
ParentClass.count #=> 3

class ChildClass < ParentClass; end

ChildClass.set_count(10)
ParentClass.count #=> 10
```

Did you expect calling `set_count` on the `Child` class to update the count in the `Parent` class? `@@` variables aren't class variables, they're _class heirarchy variables_. They almost behave more like global variables across the class hierarchy than as a class variable.

So what do you do if you want class variable semantics in ruby?

```ruby
class ParentClass
  class << self
    def set_count(count)
      @count = count
    end

    def count
      @count
    end
  end
end

ParentClass.set_count(3)
ParentClass.count #=> 3

class ChildClass < ParentClass; end

ChildClass.set_count(10)
ParentClass.count #=> 3
ChildClass.count #=> 10
```

There's a ton of great content out there explaining eigenclasses. Here's a good place to start: [https://www.devalot.com/articles/2008/09/ruby-singleton](https://www.devalot.com/articles/2008/09/ruby-singleton).

---
layout: post
title: "Some Ruby Patterns"
date: 2018-05-16
---
> Then they come up to me and say, "I was surprised by this feature of the language, so therefore Ruby violates the principle of least surprise." Wait. Wait. The principle of least surprise is not for you only. The principle of least surprise means principle of least my surprise. And it means the principle of least surprise after you learn Ruby very well.

> Yukihiro Matsumoto [https://www.artima.com/intv/ruby4.html][1]

Over 4 years of working with Ruby I've picked up a fair number of conventions, tricks and footguns. Here are a few I wish I knew when I was getting started.

---

### `||=`
Commonly used to assign a variable who's current value is nil. Takes advantage of the fact that `nil` evaluates as `false` in Ruby.

```ruby
foo = nil
foo ||= 1
foo ||= 2 # does not assign 2
foo == 1 #=> true
```

### `Symbol#to_proc`
One of the less immediately intuitive keywords in Ruby is `&:symbol`, which is a shorthand for `Symbol#to_proc` ([documentation][2]). 

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
end
```

### `&&` vs `and`, `||` vs `or`
When I was mentoring interns at Scribd, a common issue was use of `and` and `or` (particularly with developers with Python backgrounds).
TODO

### `class << self`
TODO: reopening class

### `*`
TODO: array expansion
```ruby
a = [1, 2]
b = [0, *a] # => [0, 1, 2]
```

[1]:	https://www.artima.com/intv/ruby4.html
[2]:	[https://ruby-doc.org/core-2.5.1/Symbol.html#method-i-to%5C_proc]

#!/usr/bin/env ruby

require "./lib/roman"

puts "Roman.XX is #{Roman.XX}"
puts "Roman.LXXIV is #{Roman.LXXIV}"

puts "Roman can also use 'send'"
print "Enter a numeral:  "
puts "The value is #{Roman.send(gets.strip)}"

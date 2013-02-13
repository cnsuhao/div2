#!/usr/bin/env ruby

RomanNumeralError = Class.new(StandardError)

class Roman
    SYMBOLS = {
        "I" => 1,
        "V" => 5,
        "X" => 10,
        "L" => 50,
        "C" => 100,
        "D" => 500,
        "M" => 1000
    }
   
    def self.method_missing name, *args
        roman = name.to_s.upcase

        vals = SYMBOLS.values_at(*roman.chars)

        # raise error for invalid numerals
        if vals.any?(&:nil?)
            msg = "#{name} invald. May only contain #{SYMBOLS.keys.join ", "}"
            raise RomanNumeralError, msg
        end

        prev = vals.first
        vals.inject do | sum, val |
            sum -= 2 * prev if prev < val
            prev = val
            sum + val
        end
    end
end

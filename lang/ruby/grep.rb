#!/usr/bin/env ruby

def grep(pattern, filename)
    pattern = Regexp.new(pattern) if not pattern.instance_of?(Regexp)
    raise TypeError, "#{filename}: Is a directory" if File.directory?(filename)

    File.open(filename, "r").each_with_index do |line, lineno|
        puts "#{filename}:#{lineno+1}: #{line.rstrip}" if pattern.match(line)
    end 
end

pattern, *files = ARGV
re = Regexp.new(pattern)

files.each do |path| 
    begin
        grep re, path 
    rescue TypeError => e
        puts "grep.rb: #{e}"
    end
end

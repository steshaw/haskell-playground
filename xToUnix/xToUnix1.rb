#!/usr/bin/env ruby

# XXX: Note that this code is broken as String#split doesn't split by lines but by whitespace.
# XXX: This is the original code from the blog post. A fixed version can be found in xToUnix3.rb.

# applies a transform_function (Proc instance) to an input_file 
# and writes it out to the output_file
def transform_file(transformer_function, input_file, output_file)
  File.open(output_file, 'w') do |f|
    f.write transformer_function.call(File.read(input_file))
  end
end

# returns a proc that splits a newline on either 
# Windows or Unix-style line endings, then joins 
# them with Unix-style line endings
def unix_newlines
  lambda {|f| f.split.join("\n")}
end

if ARGV.length != 2
  raise(
    StandardError, 
    "You must provide two file names on the command line!"
  )
else
  transform_file unix_newlines, ARGV.first, ARGV.last
end

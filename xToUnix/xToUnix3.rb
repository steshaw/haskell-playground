#!/usr/bin/env ruby

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
  lambda {|f| f.split(/\r\n|\r|\n/).join("\n")}
end

if ARGV.length != 2
  raise(
    StandardError, 
    "You must provide two file names on the command line!"
  )
else
  transform_file unix_newlines, ARGV.first, ARGV.last
end

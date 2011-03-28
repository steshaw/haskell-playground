#!/usr/bin/ruby

if ARGV.size == 2
  lines = File.read(ARGV[0]).gsub(/(\r\n|\r)/, "\n")
  File.open(ARGV[1], "w") { |f|
    f.write lines
  }
else
  raise StandardError, "You must provide two file names on the command line!"
end

documentation = `ri -a MiniTest::Assertions`
regex = /(?:assert|refute|flunk|pass|skip)\w*\(.*\)/
line_1 = "# -*- mode: snippet; require-final-newline: nil -*-"
line_2 = "# name: %s"
line_3 = "# key: %s"
line_4 = "# --"
dir = ARGV.first
exit if documentation.empty?
assertions = documentation.scan(regex)
Dir.mkdir(dir) unless Dir.exist?(dir)
assertions.each do |assertion|
  #  puts "Snippet for " << assertion
  file_name = assertion[/.+?(?=\()/]
  #  puts "File name: " << file_name
  snippet = assertion.gsub(/(?<=\(|, ).+?(?=\)\s*|,\s*)/).with_index do |m, i|
    "${#{1+i}:#{m}}"
  end
  snippet = snippet << "$0"
  flat_snippet = snippet.gsub(/\(/, " ").gsub(/\)/, "")
  #  puts snippet
  File.open(dir + ("/#{file_name}_p"),"w") do |file|
    file.puts(line_1)
    file.puts(sprintf(line_2, file_name + "()"))
    file.puts(sprintf(line_3, file_name + "()"))
    file.puts(line_4)
    file.write(snippet)
  end
  File.open(dir + ("/#{file_name}"), "w") do |file|
    file.puts(line_1)
    file.puts(sprintf(line_2, file_name))
    file.puts(sprintf(line_3, file_name))
    file.puts(line_4)
    file.write(flat_snippet)
  end
end

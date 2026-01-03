require "open3"

system("dune build")

BUILD = "_build/default/bin/main.exe"

def pass(file, message = "")
  puts "\e[32m#{file}: PASS #{message}\e[0m"
end

def failure(file, message = "")
  puts "\e[31m#{file}: FAIL #{message}\e[0m"
end

files = Dir.glob("test/**/*.rgel")
files.each do |file|
  test_info = File.readlines(file).first(3)
  next if test_info[0].strip.include?("skip")
  result = test_info[0].split(":").last.strip
  output = test_info[1].split(":").last.strip
  stdout, status = Open3.capture2("#{BUILD} -entry main -quickjs #{file}")
  if status.success?
    if result == "success"
      if stdout.strip =~ Regexp.new(output)
        pass(file)
      else
        failure(file, "output mismatch")
      end
    else
      failure(file, "expected failure")
    end
  else
    if result == "error"
      pass(file)
    else
      failure(file, "unexpected error")
    end
  end
end

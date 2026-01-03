require "open3"

BUILD = "_build/default/bin/main.exe"

all_files = Dir.glob("tests/**/*.rgel")

all_files.each do |file|
  metadata = {}
  lines = File.readlines(file).first(2)
  expected_status = lines[0].split(":").last.strip
  next if expected_status == "skip"
  expected_output = lines[1].split(":").last.strip
  output_type =
    if lines[1].include?("output-regex")
      :regex
    else
      :include
    end

  puts expected_status
  puts expected_output
  puts output_type
  # stdout, status = Open3.capture2("#{BUILD} -entry main -quickjs #{file}")

  # puts
  # puts stdout
  # puts

  # if status.success?
  #   puts "\e[32mPASS\e[0m #{file}"
  # else
  #   puts "\e[31mFAIL\e[0m #{file}"
  # end
end

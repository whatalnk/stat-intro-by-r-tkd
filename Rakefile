require 'pathname'
require 'fileutils'

desc "README.md => docs/index.html"
file "docs/index.html" => "README.md" do |t|
  md = "README.md"
  html = File.join(["docs", "index.html"])
  system("pandoc", *[md, "-s", "-t" "html5", "-c", "stylesheet.css", "-o", html])
end

SOURCE_FOR_GHPAGES = Rake::FileList.new("notebooks/*.html")
FILE_FOR_GHPAGES = SOURCE_FOR_GHPAGES.pathmap("docs/%f")
desc "notebooks/*.html => docs/*.html"
task :ghpages => FILE_FOR_GHPAGES
SOURCE_FOR_GHPAGES.each do |f|
  docs_file = f.pathmap("docs/%f")
  file docs_file => f do |t|
    FileUtils.copy(f, docs_file, {:preserve => true, :verbose => true})
  end
end
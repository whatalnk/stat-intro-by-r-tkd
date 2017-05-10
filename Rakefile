require "pathname"

desc "README.md => docs/index.html"
file "docs/index.html" => "README.md" do |t|
  md = "README.md"
  html = File.join(["docs", "index.html"])
  system("pandoc", *[md, "-s", "-t" "html5", "-c", "stylesheet.css", "-o", html])
end
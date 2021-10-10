elm make (gci src\*.elm | select -Expand FullName) --optimize --output src\elm.bundle.js
start src/index.html

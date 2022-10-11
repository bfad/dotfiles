#!/bin/zsh

# Pass in the path to the project or folder
rb_version=${$(head -n 1 $1/.ruby-version || echo -n '3.0.3')#ruby-}
/Users/bladmin/code/devtools/ruby-$rb_version/bin/solargraph stdio

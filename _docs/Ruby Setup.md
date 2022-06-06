# Ruby Setup

## LSP with solargraph

I want each project to use solargraph with the version of Ruby the project itself works with. To do so, I have created a folder with a Gemfile for each version of Ruby and used Bundler to install solargraph and a binstub for it. This actually allows me to locally install other dev tools I may want to run (such as overcommit or even Rails) without adding the gems to the global Ruby gems.

Here's the steps I followed (make sure to unindent the cat that wraps multiple lines):
		$> RUBY_VERSION=3.0.3
		$> mkdir -p "/Users/bladmin/code/devtools/ruby-$RUBY_VERSION"
		$> echo -n $RUBY_VERSION > "/Users/bladmin/code/devtools/ruby-$RUBY_VERSION/.ruby-version"
		$> cd "/Users/bladmin/code/devtools/ruby-$RUBY_VERSION"
		$> cat <<EOF > Gemfile
		# frozen_string_literal: true

		source 'https://rubygems.org'
		ruby '$RUBY_VERSION'

		# Dev tools I want to run without installing the gems into Ruby's global gems.
		# I ran \\\`binit\\\`, which is an alias to setup to configure installing gems locally
		# in "vender/ruby", and then \\\`bundle install --binstubs\\\` to get executables.

		gem 'solargraph'
		EOF
		$> binit --binstubs
		
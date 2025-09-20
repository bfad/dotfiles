---
applyTo: '**/*.rb'
---
# Ruby Guidelines
- Follow the style guide configuration (.rubocop.yml) which you can verify by running `bundle exec rubocop [path/to/file]` at the root of the project.
- Prefer using attributes over instance variables unless you have a very good reason. Essentially, instance variables should only be used when writing custom getters / setters and sometimes in the initialization method.
- When writing tests, only use mocks and stubs for code that makes HTTP requests or wraps an API client.
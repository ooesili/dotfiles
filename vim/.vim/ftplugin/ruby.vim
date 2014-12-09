" ruby syntax hack
if !exists("g:ruby_syntax_hack")
    let g:ruby_syntax_hack = "1"
    set filetype=eruby
    set filetype=ruby
endif

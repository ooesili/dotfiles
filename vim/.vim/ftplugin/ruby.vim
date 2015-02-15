" ruby syntax hack
" make sure we don't recursively source this file
if !exists("b:ruby_syntax_hack")
    let b:ruby_syntax_hack = "1"
    " allow function local variables
    function RubySyn()
        let l:old_ft = &filetype
        set filetype=eruby
        set filetype=ruby
        let &filetype=l:old_ft
    endfunction
    " call and delete
    call RubySyn()
    delfunction RubySyn
    unlet b:ruby_syntax_hack
endif

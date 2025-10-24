noremap P P`[
noremap . .`[
set paste
noremap U <C-r>
noremap H J
noremap J L
noremap K H
noremap L <nop>

function! NoSearch()
    let @/ = ''
    nohlsearch
endfunction
cabbrev nos
    \ <c-r>=(getcmdtype()==':' && getcmdpos()==1
    \        ? 'call NoSearch()'
    \        : 'nos')<CR>

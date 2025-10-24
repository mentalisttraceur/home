noremap P P`[
noremap . .`[
noremap U <C-r>
noremap H J
noremap J L
noremap K H
noremap L <nop>

set shada=

function! NoSearch()
    let @/ = ''
    nohlsearch
endfunction
cabbrev nos
    \ <c-r>=(getcmdtype()==':' && getcmdpos()==1
    \        ? 'call NoSearch()'
    \        : 'nos')<CR>

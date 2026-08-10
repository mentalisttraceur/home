noremap P P`[
noremap . .`[
noremap U <C-r>
noremap H J
noremap J L
noremap K H
noremap L <nop>

set virtualedit=onemore
autocmd InsertLeave * :normal! `^

inoremap <expr> <Esc> mode(1) =~# '^R' ? "\<Insert>" : "\<Esc>"

set shada=

function! NoSearch()
    let @/ = ''
    nohlsearch
endfunction
cabbrev nos
    \ <c-r>=(getcmdtype()==':' && getcmdpos()==1
    \        ? 'call NoSearch()'
    \        : 'nos')<CR>

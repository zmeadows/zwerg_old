let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Code/mine/zwerg
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +1 \!/bin/zsh
badd +1 lib/Zwerg/Component.hs
badd +111 lib/Zwerg/Component/Base.hs
badd +1 lib/Zwerg/Util.hs
badd +39 lib/Zwerg/Generator/Level/Cave.hs
badd +25 lib/Zwerg/UI/GlyphMap.hs
badd +21 lib/Zwerg/Generator/Enemy/Goblin.hs
badd +199 lib/Zwerg/Game.hs
badd +109 lib/Zwerg/UI/Menu.hs
badd +4 TODO
badd +18 lib/Zwerg/Geometry/FOV.hs
badd +91 lib/Zwerg/Event.hs
badd +171 lib/Zwerg/Entity.hs
badd +48 lib/Zwerg/Generator/Default.hs
badd +21 lib/Zwerg/Generator/Verify.hs
badd +15 lib/Zwerg/Generator/Item/Weapon.hs
argglobal
silent! argdel *
edit lib/Zwerg/UI/Menu.hs
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winminheight=1 winheight=1 winminwidth=1 winwidth=1
wincmd =
argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 109 - ((79 * winheight(0) + 41) / 83)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
109
normal! 09|
wincmd w
argglobal
edit lib/Zwerg/Game.hs
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 50 - ((49 * winheight(0) + 41) / 83)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
50
normal! 082|
wincmd w
argglobal
edit lib/Zwerg/Generator/Level/Cave.hs
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 93 - ((71 * winheight(0) + 41) / 83)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
93
normal! 028|
wincmd w
3wincmd w
wincmd =
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
set winminheight=1 winminwidth=1
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
let g:this_session = v:this_session
let g:this_obsession = v:this_session
let g:this_obsession_status = 2
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :

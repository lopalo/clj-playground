" :Piggieback :patch
autocmd BufWritePost *.clj,*.cljs,*.cljc :Require
autocmd BufWritePost *.cljs,*.cljc silent :Eval (clj-playground.core/mount!)

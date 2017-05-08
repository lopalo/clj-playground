" :Piggieback :patch
autocmd BufWritePost *.cljs,*.cljc silent :Eval (clj-playground.core/mount!)

###### scoresway functions

match_search=function(i,n) {
  seq.int(i-n,i,length.out=n+1)
}

first_match=function(compn,g) {
  g %>% filter(comp==compn) %>% summarize(f=first(match)) %>% pull(f)
}

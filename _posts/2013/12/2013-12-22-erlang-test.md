---
layout: post
category : Erlang
tags : [code, erlang]
---


## 这是一个测试用的页面 2

    loop([H|T]) ->
    	doSome(H),
    	loop(T);
    loop([]) ->
    	end.



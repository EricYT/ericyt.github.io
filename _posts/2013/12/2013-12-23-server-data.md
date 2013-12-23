---
layout: post
category : Erlang
tags : [code, erlang, game]
descriptions : ETS data manager
---



# 游戏服务端数据相关 #

*简单的阐述一下服务端对于ETS内存表的管理策略。使用的是AB表的原理*



1.  在节点启动的同时，会根据配置选择需要将哪些数据初始化进入内存（ETS）。内存中的数据维护两份，一份是数据的备份数据。



2. 两份数据在正常情况下，完全相同。会有一个flag来控制当前游戏服务器使用的是那一份数据。这样做的目的是使得服务器需要更新数据时，可以不用中止当前对数据层的访问。方法如下：  
 1. 判断当前服务器正在使用的数据版本，找到另一个闲置的版本；  
 2. 更新闲置版本；  
 3. 将使用版本切换到当前闲置版本（即刚更新过的数据版本）；  
 4. 同步另一个版本数据为当前版本数据。  

3. 在数据初始化时，会将整个文件读入，数据都是按Erlang的record来存储的，如下形式：


>  %%data version 1.0  
> ｛proto_item_template, 10, 10, [1, 2, 3].  
> ｛proto_item_template, 10, 10, [1, 2, 3]}.



首先会将其**`file:read_file`**出来，将其二进制内容使用计算其md5值，基本操作如下：


    get_md5_data() ->
     case file:read_file(FileName) of
      {ok, ConfigData} ->
       ok;
      _ ->
       %% error read file
       ConfigData = <<>>
       do_some_log
     end,
     md5_hex(ConfigData).
    
    md5_hex(S) ->
     Md5_bin = erlang:md5(S),
     Md5_list = erlang:binary_to_list(Md5_bin),
     lists:flatten(list_to_hex(Md5_list)).
    
    list_to_hex(L) ->
     lists:map(fun(X) -> int_to_hex(X) end, L).
    
    int_to_hex(N) when N < 256 ->
     [hex(N div 16), hex(N rem 16].
    
    hex(N) when N < 10 ->
     $0 + N;
    hex(N) when N >= 10, N < 16 ->
     $a + N.


但是为何要将md5值转换为16进制的呢？存储这样一个值的目的在于如果数据发生变化后可以根据新生成的文件的md5判断两个版本是否不一样，从而进行数据的更新。而不至于通过使用文件的比对来判断是否发生了变化。
整个数据会在ets中存储一份，ets为bug类型。

这样在以每一个record创建单独的ets表时就可以直接从其中取数据，而不用再次从db读数据。

每一个上述的record都会在生成的时候，自动生成访问层的代码。其会根据相应的配置文件生成相应的代码。




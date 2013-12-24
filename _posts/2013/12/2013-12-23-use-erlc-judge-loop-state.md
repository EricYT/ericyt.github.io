---
layout: post
category : Erlang
tags : [code, erlang, game]
descriptions : Judge a loop state
---



# 通过erlc判断递归函数是否为尾递归 #

*使用erlc编译erlang source code为字节码，通过字节码的特征来判断这个递归是否为尾递归*


----------

erlc的相关介绍可以到erlang的文档中找到，这里就不细说。这里主要会使用到的就是 `-S` 选项。直接上代码  

<?prettify?>
<pre class="prettyprint linenums">
-module(test_file).

-export([test1/1, test2/1, test_case/1]).


test1([Head|Tail]) -&gt;
    test1(Tail);
test1([]) -&gt;
    ok.

test2([Head|Tail]) -&gt;
    test2(Tail),
    ok;
test2([]) -&gt;
    ok.


test_case([Head|Tail]) -&gt;
    case Head of
    	Head when is_integer(Head) -&gt;
    		test_case(Tail);
    	_ -&gt;
    	ok
    end;
test_case([]) -&gt;
    ok.
</pre>

使用erlc -S生成的字节码

<?prettify?>
<pre class="prettyprint linenums">
{module, test_file}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{test1,1},{test2,1},{test_case,1}]}.

{attributes, []}.

{labels, 15}.


{function, test1, 1, 2}.
  {label,1}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",6}]}.
    {func_info,{atom,test_file},{atom,test1},1}.
  {label,2}.
    {test,is_nonempty_list,{f,3},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,0}}.
    {call_only,1,{f,2}}.
  {label,3}.
    {test,is_nil,{f,1},[{x,0}]}.
    {move,{atom,ok},{x,0}}.
    return.


{function, test2, 1, 5}.
  {label,4}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",11}]}.
    {func_info,{atom,test_file},{atom,test2},1}.
  {label,5}.
    {test,is_nonempty_list,{f,6},[{x,0}]}.
    {allocate,0,1}.
    {get_list,{x,0},{x,1},{x,0}}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",12}]}.
    {call,1,{f,5}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,6}.
    {test,is_nil,{f,4},[{x,0}]}.
    {move,{atom,ok},{x,0}}.
    return.


{function, test_case, 1, 8}.
  {label,7}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",18}]}.
    {func_info,{atom,test_file},{atom,test_case},1}.
  {label,8}.
    {test,is_nonempty_list,{f,10},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_integer,{f,9},[{x,1}]}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,8}}.
  {label,9}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,10}.
    {test,is_nil,{f,7},[{x,0}]}.
    {move,{atom,ok},{x,0}}.
    return.


{function, module_info, 0, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,test_file},{atom,module_info},0}.
  {label,12}.
    {move,{atom,test_file},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 14}.
  {label,13}.
    {line,[]}.
    {func_info,{atom,test_file},{atom,module_info},1}.
  {label,14}.
    {move,{x,0},{x,1}}.
    {move,{atom,test_file},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.

</pre>


由此可见，当函数调用中使用的是一般的**call**的时候，这个递归就是非尾递归的。是会有堆栈的消耗的。当函数的调用是**call_only**时，说明就是尾递归的。



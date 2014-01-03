---
layout: post
category : Erlang
tags : [code, erlang, game]
descriptions : Judge a loop state
---



# 通过erlc理解`-compile`中内联函数的工作原理 #

*使用erlc编译erlang source code为字节码，通过字节码的特征来判断内联函数的工作原理*


----------

erlc的相关介绍可以到erlang的文档中找到，这里就不细说。这里主要会使用到的就是 `-S` 选项。直接上代码  

<?prettify?>
<pre class="prettyprint linenums">

-module(test_file).

-export([test/0, test1/0, test2/0]).

-spec test() -&gt; ignore.
test() -&gt;
	io:format("Hello world~n").
	
-spec test1() -&gt; ignore.
test1() -&gt;
	do_some().
	
-spec test2() -&gt; ignore.
test2() -&gt;
	do_some1().

-compile({inline, [do_some/0]}).
do_some() -&gt;
	io:format("Hello world~n").

do_some1() -&gt;
	io:format("Hello world~n").

</pre>

使用erlc -S生成的字节码

<?prettify?>
<pre class="prettyprint linenums">
{module, test_file}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{test,0},{test1,0},{test2,0}]}.

{attributes, []}.

{labels, 13}.


{function, test, 0, 2}.
  {label,1}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",6}]}.
    {func_info,{atom,test_file},{atom,test},0}.
  {label,2}.
    {move,{literal,"Hello world~n"},{x,0}}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",7}]}.
    {call_ext_only,1,{extfunc,io,format,1}}.


{function, test1, 0, 4}.
  {label,3}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",10}]}.
    {func_info,{atom,test_file},{atom,test1},0}.
  {label,4}.
    {move,{literal,"Hello world~n"},{x,0}}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",19}]}.
    {call_ext_only,1,{extfunc,io,format,1}}.


{function, test2, 0, 6}.
  {label,5}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",14}]}.
    {func_info,{atom,test_file},{atom,test2},0}.
  {label,6}.
    {call_only,0,{f,8}}.


{function, do_some1, 0, 8}.
  {label,7}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",21}]}.
    {func_info,{atom,test_file},{atom,do_some1},0}.
  {label,8}.
    {move,{literal,"Hello world~n"},{x,0}}.
    {line,[{location,"e:/GitHub/CODE_T~1/test_file.erl",22}]}.
    {call_ext_only,1,{extfunc,io,format,1}}.


{function, module_info, 0, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,test_file},{atom,module_info},0}.
  {label,10}.
    {move,{atom,test_file},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,test_file},{atom,module_info},1}.
  {label,12}.
    {move,{x,0},{x,1}}.
    {move,{atom,test_file},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.

</pre>


生成的字节码，能够清楚的看出来。使用inline编译的函数，在最终调用的过程中被原样的替换到代码中去。而如果是函数的调用则还是按照正常的函数调用方式。



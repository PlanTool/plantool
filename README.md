# plantool
## automated planning toolbox

1. 目前找到的源代码都放在code里面了，大家选择或者去找自己的两到三个，根据可执行文件
的形式（输入的参数设置，输出结果等），把找到的源代码修改用python调用起来。

2. 编译生成动态链接库的方法：
>>2.1 打开源代码的makefile文件，把需要生成的.c文件手动生成（PS：记得按照文件的依赖关系，
顺序编译，否则会报错）
>>2.2 使用命令（以FF-v2.3为例）生成动态链接库
```bash
gcc -o ff.so -shared -fPIC main.c memory.c output.c parse.c inst_pre.c inst_easy.c inst_hard.c inst_final.c orderings.c relax.c search.c scan-fct_pddl.tab.c scan-ops_pddl.tab.c
```
>>2.3 （测试）进入python，输入命令
```python
import ctypes
import _ctypes

lib = ctypes.CDLL('libs/ipp.so')
argv = ['ff', '-o', 'pddl_files/ff_domain.pddl', '-f', 'pddl_files/ff_problem.pddl']
lib.main(len(argv), (ctypes.c_char_p*len(argv))(*argv))

_ctypes.dlclose(lib._handle)
```
>>2.4 编译.so文件时包含的.c文件里面的函数都可以调用，方法类似，如
```python
lib.ff_usage()
```

3. 用wxpython写的更新版GUI已經放在GUI文件夾中，增加了比較多功能，你們可以參考一下。



                                                                2017年3月27日

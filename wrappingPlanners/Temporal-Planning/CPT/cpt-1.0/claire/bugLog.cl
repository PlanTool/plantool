// bug file: we keep track of all changes between version
// author: Yves Caseau
//

Conventions

   bug number:  <version>.<id>
   critical:    1: critical (deployed application cannot work)
                2: serious  (developped application cannot work)
                3: normal (a fix may be found)
                4: modification request


+--------+----+----------+--------+----------------------------------------------+
| bug #  |CR  | open     | closed |       comment                                |
+--------+----+----------+--------+----------------------------------------------+
+--------------------------------------------------------------------------------+
| release: 3.2.00  - start bug tracking - 14/8/01                                |
+--------+----+----------+--------+----------------------------------------------+
|  3.2.1 | 4  | 14/8/01  |        | API to allow the GC to free imported objects |
|        |    | sylvain  |        |                                              |                                              |
|  3.2.2 | 2  | 28/8/01  |        | 2 bugs with Store example from doc           |
|        |    | EGa      | bug7   |                                              |                                              |
|  3.2.3 | 3  | 28/8/01  |        | type inference problem with tuples           |
|        |    | Fla      | bug12  |                                              |                                              |
|  3.2.4 | 2  | 8/11/01  | x      | safe(x + y) not supported !                  |
|        |    | EBO      | bug14  |                                              |                                              |
|  3.2.5 | 1  | 9/11/01  | x      | default value for string slots               |
|        |    | SBenilan | bug8   |                                              |                                              |
|  3.2.6 | 3  | 21/11/01 | x      | block(n) under debuger fails !               |
|        |    | EBO      |        |                                              |                                              |
|  3.2.7 | 1  | 21/11/01 | x      | reader bug: type without ( ) -> GPF          |
|        |    | FXJ      |        |                                              |                                              |
|  3.2.8 | 2  | 21/11/01 |        | LINUX installation bug *fs/fe* not           |
|        |    | MlM      |        |                                              |                                              |
|  3.2.9 | 2  | 21/11/01 | x      | Type inference for collect                   |
|        |    | FLA      | bug14  |                                              |                                              |
|  3.2.10| 2  | 8/1/02   | x      | default value = string with new              |
|        |    | SBn      |        |                                              |                                              |
|  3.2.11| 3  | 12/1/02  | x      | bug with floats in inspector                 |
|        |    | ThB      |        |                                              |                                              |
|  3.2.12| 2  | 10/1/02  | x      | tuple stack optimization did not work        |
|        |    | ThB      | bug15  |                                              |                                              |
|  3.2.13| 2  | 5/1/02   | x      | new(C) != C() from default values p.o.v.     |
|        |    | SBn      |        |                                              |                                              |
|  3.2.14| 1  | 24/1/02  | x      | Compiler crashes with (x % tuple(....)) !    |
|        |    | Ali      |        |                                              |                                              |
|  3.2.15| 2  | 23/1/02  | x      | protection of float constants does not       |
|        |    | Ali      | bug14  | work if the container is x:any !             |                                              |
|  3.2.16| 3  | 22/1/02  | x      | list!(a:array) missed the type               |
|        |    | ThB      |        |                                              |                                              |
|  3.2.17| 2  | 20/1/02  | x      | complex list expression produces C++ error   |
|        |    | Fla      | bug12  |                                             |                                              |
|  3.2.  | i  | <date>   | done?  | <description>                                |
|        |    | <author> | file   |                                              |                                              |



How to submit a bug:
--------------------

  send to clairebugs@yahoo.fr
     - a description of the bug
     - a reference to the claire version that you are using and the
       associated computing environment
     - a self-contained file that reproduces the bug.



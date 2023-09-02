(rule
 (with-stdout-to assignment.out.lua (run %{bin:luafmt} %{dep:cases/assignment.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/assignment.exp.lua assignment.out.lua)))

(rule
 (with-stdout-to comment.out.lua (run %{bin:luafmt} %{dep:cases/comment.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/comment.exp.lua comment.out.lua)))

(rule
 (with-stdout-to comment_alone.out.lua (run %{bin:luafmt} %{dep:cases/comment_alone.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/comment_alone.exp.lua comment_alone.out.lua)))

(rule
 (with-stdout-to comment_before.out.lua (run %{bin:luafmt} %{dep:cases/comment_before.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/comment_before.exp.lua comment_before.out.lua)))

(rule
 (with-stdout-to empty.out.lua (run %{bin:luafmt} %{dep:cases/empty.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/empty.exp.lua empty.out.lua)))

(rule
 (with-stdout-to empty_spaces.out.lua (run %{bin:luafmt} %{dep:cases/empty_spaces.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/empty_spaces.exp.lua empty_spaces.out.lua)))

(rule
 (with-stdout-to function_def.out.lua (run %{bin:luafmt} %{dep:cases/function_def.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/function_def.exp.lua function_def.out.lua)))

(rule
 (with-stdout-to string.out.lua (run %{bin:luafmt} %{dep:cases/string.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/string.exp.lua string.out.lua)))

(rule
 (with-stdout-to table.out.lua (run %{bin:luafmt} %{dep:cases/table.in.lua})))

(rule
 (alias runtest)
 (action (diff cases/table.exp.lua table.out.lua)))
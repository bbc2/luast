(rule
 (with-stdout-to assignment.out (run %{bin:luafmt} %{dep:cases/assignment.lua})))

(rule
 (alias runtest)
 (action (diff cases/assignment.exp assignment.out)))

(rule
 (with-stdout-to comment.out (run %{bin:luafmt} %{dep:cases/comment.lua})))

(rule
 (alias runtest)
 (action (diff cases/comment.exp comment.out)))

(rule
 (with-stdout-to comment_alone.out (run %{bin:luafmt} %{dep:cases/comment_alone.lua})))

(rule
 (alias runtest)
 (action (diff cases/comment_alone.exp comment_alone.out)))

(rule
 (with-stdout-to empty.out (run %{bin:luafmt} %{dep:cases/empty.lua})))

(rule
 (alias runtest)
 (action (diff cases/empty.exp empty.out)))

(rule
 (with-stdout-to empty_spaces.out (run %{bin:luafmt} %{dep:cases/empty_spaces.lua})))

(rule
 (alias runtest)
 (action (diff cases/empty_spaces.exp empty_spaces.out)))

(rule
 (with-stdout-to string.out (run %{bin:luafmt} %{dep:cases/string.lua})))

(rule
 (alias runtest)
 (action (diff cases/string.exp string.out)))

(rule
 (with-stdout-to table.out (run %{bin:luafmt} %{dep:cases/table.lua})))

(rule
 (alias runtest)
 (action (diff cases/table.exp table.out)))
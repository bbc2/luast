(rule
 (with-stdout-to assignment.out (run %{bin:luafmt} %{dep:cases/assignment.lua})))

(rule
 (alias runtest)
 (action (diff cases/assignment.exp assignment.out)))

(rule
 (with-stdout-to empty.out (run %{bin:luafmt} %{dep:cases/empty.lua})))

(rule
 (alias runtest)
 (action (diff cases/empty.exp empty.out)))

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
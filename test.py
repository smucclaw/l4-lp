from javascript import require, globalThis
from edn_format import Symbol
import edn_format as edn_format
import janus_swi as janus

l4_lp = require("./public/js/node_lib.js")

program_str = '''
  [(DECIDE p of var/xs and var/x
  IF (var/xs IS THE LIST OF ALL var/y SUCH THAT q holds for var/y)
  AND (var/x IS THE SUM OF var/xs))

  (DECIDE q holds for 1)
  (DECIDE q holds for 2)]
'''

program_edn = [
  (
    Symbol("DECIDE"), Symbol("p"), Symbol("of"), Symbol("var/xs"),
    Symbol("and"), Symbol("var/x"),
    Symbol("IF"), (
      Symbol("var/xs"), Symbol("IS"), Symbol("THE"), Symbol("LIST"),
      Symbol("OF"), Symbol("ALL"), Symbol("var/y"), Symbol("SUCH"),
      Symbol("THAT"), Symbol("q"), Symbol("holds"), Symbol("for"),
      Symbol("var/y")
   ),
    Symbol("AND"), (
      Symbol("var/x"), Symbol("IS"), Symbol("THE"), Symbol("SUM"), Symbol("OF"),
      Symbol("var/xs")
    )
  ),

  (Symbol("DECIDE"), Symbol("q"), Symbol("holds"), Symbol("for"), 1),
  
  (Symbol("DECIDE"), Symbol("q"), Symbol("holds"), Symbol("for"), 2)
]

assert edn_format.loads(program_str) == program_edn
# assert program_str == edn_format.dumps(program_edn)

print(edn_format.dumps(program_edn))

goal = "(p of var/xs and var/x)"

program = l4_lp.l4_program_to_prolog_program_str(program_str)
goal = l4_lp.l4_to_prolog_str(goal)

print(f'Transpiled program: {program}')
print(f'Transpiled goal: {goal}')

janus.attach_engine()

janus.consult("public/resources/swipl/prelude.qlf")
janus.consult("program", program)

result = janus.query_once(goal)
print(f'Result: {result}')

janus.detach_engine()
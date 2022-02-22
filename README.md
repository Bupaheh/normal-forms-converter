# Normal forms converter
This project converts propositional formula into three normal forms: NNF, DNF, CNF.

## Example

```bash
$ stack run
a <=> ~ (a & b => c | d)
NNF: (~a | a & b & ~c & ~d) & (~a | ~b | c | d | a)
DNF: ~a & ~a | a & b & ~c & ~d & ~a | ~a & ~b | a & b & ~c & ~d & ~b | ~a & c | a & b & ~c & ~d & c | ~a & d | a & b & ~c & ~d & d | ~a & a | a & b & ~c & ~d & a
CNF: (~a | a) & (~a | b) & (~a | ~c) & (~a | ~d) & (~a | ~b | c | d | a)
```
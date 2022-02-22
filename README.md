# Normal forms converter
This project converts propositional formula into three normal forms: NNF, DNF, CNF.

## Example

```bash
$ stack run
a <=> (a & b => c | d)
NNF: (~a | ~a | ~b | c | d) & (a & b & ~c & ~d | a)
DNF: ~a & a & b & ~c & ~d | ~a & a & b & ~c & ~d | ~b & a & b & ~c & ~d | c & a & b & ~c & ~d | d & a & b & ~c & ~d | ~a & a | ~a & a | ~b & a | c & a | d & a
CNF: (~a | ~a | ~b | c | d) & (a | a) & (b | a) & (~c | a) & (~d | a)
```
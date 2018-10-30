#ifndef X64_H
#define X64_H

#define FOR_CALLEE_SAVE(op, sep)		\
	op(1, rbx) sep				\
	op(2, r12) sep				\
	op(3, r13) sep				\
	op(4, r14) sep				\
	op(5, r15)

#define FOR_CALLER_SAVE(op, sep)		\
	op(1, rdx) sep                          \
	op(2, rcx) sep                          \
	op(3, r8) sep				\
	op(4, r9) sep				\
	op(5, r10)

#endif  /* X64_H */

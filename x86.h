#ifndef X86_H
#define X86_H

#define FOR_CALLEE_SAVE(op, sep)		\
	op(1, ebx) sep				\
	op(2, esi)

#define FOR_CALLER_SAVE(op, sep)		\
	op(1, edx) sep				\
	op(2, edi)

#endif  /* X86_H */

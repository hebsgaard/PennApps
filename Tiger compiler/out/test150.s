
	.text
# PROCEDURE tigermain
	.globl	tigermain
	.func	tigermain
	.type	tigermain, @function
tigermain:
	# FRAME tigermain(1 formals, 23 locals)
	pushl %ebp
	movl %esp, %ebp
	subl $96, %esp
	# SP, FP, calleesaves, argregs have values
L10_blocks:                                       # x86gen:128
	movl %ebp, -12(%ebp)                      # x86gen:277 x86frame:629
	movl -12(%ebp), %ebx                      # x86gen:265 x86frame:386
	addl $-8,  %ebx                           # x86gen:265 x86frame:392
	movl %ebx, -12(%ebp)                      # x86gen:265 x86frame:398
	movl -12(%ebp), %ebx                      # x86gen:122 x86frame:647
	movl %ebx, -28(%ebp)                      # x86gen:122 x86frame:653
	movl -16(%ebp), %ebx                      # x86gen:407 x86frame:325
	movl $7, %ebx                             # x86gen:407 x86frame:330
	movl %ebx, -16(%ebp)                      # x86gen:407 x86frame:336
	movl -16(%ebp), %ebx                      # x86gen:218 x86frame:266
	pushl %ebx                                # x86gen:218 x86frame:271
	movl -20(%ebp), %ebx                      # x86gen:407 x86frame:325
	movl $12, %ebx                            # x86gen:407 x86frame:330
	movl %ebx, -20(%ebp)                      # x86gen:407 x86frame:336
	movl -20(%ebp), %ebx                      # x86gen:218 x86frame:266
	pushl %ebx                                # x86gen:218 x86frame:271
	call initArray                            # x86gen:70
	addl $8, %esp                             # x86gen:57
	movl %eax, -24(%ebp)                      # x86gen:72 x86frame:629
	movl -28(%ebp), %ebx                      # x86gen:119 x86frame:647
	movl %ebx, -24(%ebp)                      # x86gen:119 x86frame:653
	movl %ebp, -32(%ebp)                      # x86gen:277 x86frame:629
	movl -32(%ebp), %ebx                      # x86gen:265 x86frame:386
	addl $-4,  %ebx                           # x86gen:265 x86frame:392
	movl %ebx, -32(%ebp)                      # x86gen:265 x86frame:398
	movl -32(%ebp), %ebx                      # x86gen:122 x86frame:647
	movl %ebx, -48(%ebp)                      # x86gen:122 x86frame:653
	movl -36(%ebp), %ebx                      # x86gen:407 x86frame:325
	movl $0, %ebx                             # x86gen:407 x86frame:330
	movl %ebx, -36(%ebp)                      # x86gen:407 x86frame:336
	movl -36(%ebp), %ebx                      # x86gen:218 x86frame:266
	pushl %ebx                                # x86gen:218 x86frame:271
	movl -40(%ebp), %ebx                      # x86gen:407 x86frame:325
	movl $12, %ebx                            # x86gen:407 x86frame:330
	movl %ebx, -40(%ebp)                      # x86gen:407 x86frame:336
	movl -40(%ebp), %ebx                      # x86gen:218 x86frame:266
	pushl %ebx                                # x86gen:218 x86frame:271
	call initArray                            # x86gen:70
	addl $8, %esp                             # x86gen:57
	movl %eax, -44(%ebp)                      # x86gen:72 x86frame:629
	movl -48(%ebp), %ebx                      # x86gen:119 x86frame:647
	movl %ebx, -44(%ebp)                      # x86gen:119 x86frame:653
	movl -52(%ebp), %ebx                      # x86gen:231 x86frame:347
	movl -8(%ebp), %ebx                       # x86gen:231 x86frame:353
	movl %ebx, -52(%ebp)                      # x86gen:231 x86frame:359
	movl -52(%ebp), %ebx                      # x86gen:100 x86frame:281
	movl %ebx, -4(%ebp)                       # x86gen:100 x86frame:286
	movl -56(%ebp), %ebx                      # x86gen:231 x86frame:347
	movl -4(%ebp), %ebx                       # x86gen:231 x86frame:353
	movl %ebx, -56(%ebp)                      # x86gen:231 x86frame:359
	movl -56(%ebp), %ebx                      # x86gen:255 x86frame:406
	movl (%ebx), %ecx                         # x86gen:255 x86frame:412
	movl %ecx, -56(%ebp)                      # x86gen:255 x86frame:418
	movl -60(%ebp), %ebx                      # x86gen:279 x86frame:647
	movl %ebx, -76(%ebp)                      # x86gen:279 x86frame:653
	movl -64(%ebp), %ebx                      # x86gen:407 x86frame:325
	movl $7, %ebx                             # x86gen:407 x86frame:330
	movl %ebx, -64(%ebp)                      # x86gen:407 x86frame:336
	movl -64(%ebp), %ebx                      # x86gen:326 x86frame:647
	movl %ebx, -72(%ebp)                      # x86gen:326 x86frame:653
	movl -68(%ebp), %ebx                      # x86gen:407 x86frame:325
	movl $4, %ebx                             # x86gen:407 x86frame:330
	movl %ebx, -68(%ebp)                      # x86gen:407 x86frame:336
	movl -68(%ebp), %ecx                      # x86gen:332 x86frame:570
	movl -72(%ebp), %ebx                      # x86gen:332 x86frame:576
	imull %ecx, %ebx                          # x86gen:332 x86frame:582
	movl %ebx, -72(%ebp)                      # x86gen:332 x86frame:588
	movl -72(%ebp), %ecx                      # x86gen:285 x86frame:570
	movl -76(%ebp), %ebx                      # x86gen:285 x86frame:576
	addl %ecx, %ebx                           # x86gen:285 x86frame:582
	movl %ebx, -76(%ebp)                      # x86gen:285 x86frame:588
	movl -76(%ebp), %ebx                      # x86gen:255 x86frame:406
	movl (%ebx), %ecx                         # x86gen:255 x86frame:412
	movl %ecx, -76(%ebp)                      # x86gen:255 x86frame:418
	movl -80(%ebp), %ebx                      # x86gen:218 x86frame:266
	pushl %ebx                                # x86gen:218 x86frame:271
	pushl %ebp                                # x86gen:218
	call printInt                             # x86gen:188
	addl $8, %esp                             # x86gen:57
	jmp L9_block_done                         # x86gen:179
L9_block_done:                                    # x86gen:128
	# FP, SP, RV, calleesaves still live
	leave
	ret
	.size	tigermain, .-tigermain
	.endfunc
# END tigermain


	.data
L0_string:
	.long 13
	.asciz "DefaultString"

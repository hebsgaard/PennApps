(*Fie Hebsgaard - Pedersen, 20103511*)
(*Supplementary project*)

Require Import Arith.

(*
"The beautiful thing about learning is that nobody can take it away from you"
- B.B. King
*)

Inductive matrix_2x2 : Type :=
  | create : nat -> nat -> nat -> nat -> matrix_2x2.

Definition matrix_1234 := 
  create 1 2 3 4.

Definition matrix_1356 := 
  create 1 3 5 6.

Definition matrix_5312 := 
  create 5 3 1 2.

Definition matrix_3475 := 
  create 3 4 7 5.

Definition specification_of_times_matrices (times : matrix_2x2 ->
                                                  matrix_2x2 ->
                                                  matrix_2x2) :=
  forall times : (matrix_2x2 -> matrix_2x2 -> matrix_2x2),
    forall a b c d e f g h : nat,
      times (create a b c d) (create e f g h) = create (a * e + b * g) (a * f + b * h) (c * e  + d * g) (c * f + d * h).

Definition times (mat1 mat2 : matrix_2x2) : matrix_2x2 :=
  match mat1 with 
    | create a b c d => match mat2 with 
                          | create e f g h =>
                            create (a * e + b * g) (a * f + b * h) (c * e  + d * g) (c * f + d * h)
                            end
  end.
(*
Compute times matrix_5312 matrix_3475.
  = create 36 35 17 14
     : matrix_2x2

Compute times matrix_1356 matrix_1234.
 = create 10 14 23 34
     : matrix_2x2 

Compute times matrix_1234 matrix_5312.
 = create 7 7 19 17
     : matrix_2x2
*)

Lemma unfold_times :
  forall a b c d e f g h : nat,  
    times (create a b c d) (create e f g h) = create (a * e + b * g) (a * f + b * h) (c * e  + d * g) (c * f + d * h).
Proof. 
  intros a b c d e f g h.
  unfold times.
  reflexivity.
Qed.

(*
Compute times matrix_5312 (times matrix_1234 matrix_1356). 
   = create 124 174 57 81
    : matrix_2x2

Compute times (times matrix_5312  matrix_1234) matrix_1356.   
    = create 124 174 57 81
     : matrix_2x2
*)   

Theorem times_is_associative :
  forall a b c d e f g h i j k l : nat, 
    times (create a b c d) (times (create e f g h) (create i j k l)) = 
    times (times (create a b c d) (create e f g h)) (create i j k l).
Proof.
  intros a b c d e f g h i j k l.
  rewrite ->4 unfold_times.
  rewrite ->8 mult_plus_distr_r.
  rewrite ->8 mult_plus_distr_l.
  rewrite ->16 mult_assoc.
  rewrite ->8 plus_assoc.
  Check plus_assoc.
  rewrite <- (plus_assoc (a * e * i)(a * f * k) (b * g * i)).
  rewrite -> (plus_comm (a * f * k) (b * g * i)).
  rewrite -> plus_assoc.
  rewrite <- (plus_assoc (a * e * j)(a * f * l) (b * g * j)).
  rewrite -> (plus_comm (a * f * l) (b * g * j)).
  rewrite -> plus_assoc.
  rewrite <- (plus_assoc (c * e * i)(c * f * k) (d * g * i)).
  rewrite -> (plus_comm (c * f * k) (d * g * i)).
  rewrite -> plus_assoc.
  rewrite <- (plus_assoc (c * e * j)(c * f * l) (d * g * j)).
  rewrite -> (plus_comm (c * f * l) (d * g * j)).
  rewrite -> plus_assoc.
  reflexivity.
Qed.

Definition I : matrix_2x2 :=
  create 1 0 0 1.

Lemma unfold_I :
  I = create 1 0 0 1.
Proof.
  unfold I.
  reflexivity.
Qed.

(*
Compute times I matrix_5312.   
     = create 5 3 1 2
     : matrix_2x2

Compute times I matrix_1234.   
     = create 1 2 3 4
     : matrix_2x2
*)

Theorem I_is_neutral_on_the_left :
  forall a b c d : nat, 
    times I (create a b c d) = create a b c d.
Proof.
  intros a b c d.
  rewrite -> unfold_I.
  rewrite -> unfold_times.
  rewrite ->4 mult_0_l.
  rewrite ->4 mult_1_l.
  rewrite ->2 plus_0_l.
  rewrite ->2 plus_0_r.
  reflexivity.
Qed.

(*
Compute times matrix_1356 I.
     = create 1 3 5 6
     : matrix_2x2
Compute times matrix_3475 I. 
     = create 3 4 7 5
     : matrix_2x2 
*)

Theorem I_is_neutral_on_the_right :
  forall a b c d : nat, 
    times (create a b c d) I = create a b c d.
Proof.
  intros a b c d.
  rewrite -> unfold_I.
  rewrite -> unfold_times.
  rewrite ->4 mult_0_r.
  rewrite ->4 mult_1_r.
  rewrite ->2 plus_0_l.
  rewrite ->2 plus_0_r.
  reflexivity.
Qed.

Definition specification_of_exponentiation_matrices (exp : matrix_2x2 -> nat ->
                                                  matrix_2x2) :=
  forall exp : (matrix_2x2 -> nat -> matrix_2x2),
    forall a b c d n: nat,
      exp (create a b c d) (S n) = times (exp (create a b c d) n) (create a b c d).

Fixpoint exp (mat1 : matrix_2x2) (n : nat) : matrix_2x2 := 
    match mat1 with 
        | create a b c d =>
          match n with
            | 0 => I
            | (S n) => times (exp (create a b c d) n) (create a b c d)
          end
    end.

(*
Compute exp matrix_5312 1.   
     = create 5 3 1 2
     : matrix_2x2

Compute exp matrix_1234 4. 
     = create 199 290 435 634
     : matrix_2x2

Compute exp matrix_1356 0. 
     = create 1 0 0 1
     : matrix_2x2

Compute exp I 3.
     = create 1 0 0 1
     : matrix_2x2
*)

Lemma unfold_exp_bc :
  forall a b c d : nat, 
    exp (create a b c d) 0 = I.
Proof.
  intros a b c d.
  unfold exp.
  reflexivity.
Qed.

Lemma unfold_exp_ic :
  forall a b c d n : nat, 
    exp (create a b c d) (S n) = times (exp (create a b c d) n) (create a b c d).
Proof.
  intros a b c d n.
  unfold exp.
  fold exp.
  reflexivity.
Qed.

Lemma unfold_plus_induction_case :
  forall i j : nat,
    (S i) + j = S (i + j).
Proof.
  intros i j.
  unfold plus.
  fold plus.
  reflexivity.
Qed.

Lemma increment :
  forall n : nat,
    S n = 1 + n.
Proof.
  intro n.
  rewrite -> unfold_plus_induction_case.
  rewrite -> plus_0_l.
  reflexivity.
Qed.

(*
Compute exp (create 1 1 0 1) 2.
     = create 1 2 0 1
     : matrix_2x2

Compute exp (create 1 1 0 1) 3.
     = create 1 3 0 1
     : matrix_2x2

Compute exp (create 1 1 0 1) 4.
     = create 1 4 0 1
     : matrix_2x2
*)

(*
"Whenever you find yourself on the side of the majority, it is time to 
pause and reflect."
- Mark Twain
*)


Theorem n_th_exponent_of_matrix_1_1_0_1 :
  forall n : nat, 
    exp (create 1 1 0 1) n = create 1 n 0 1.
Proof.
  intro n.
  induction n as [ | n' IHn'].
  
  rewrite -> unfold_exp_bc.
  rewrite -> unfold_I.
  reflexivity.

  rewrite -> unfold_exp_ic.
  rewrite -> IHn'.
  rewrite -> unfold_times.
  rewrite ->2 mult_0_r.
  rewrite ->3 mult_1_r.
  rewrite ->2 plus_0_l.
  rewrite -> plus_0_r.
  rewrite -> (increment n'). 
  reflexivity.
Qed.

Definition specification_of_Fibonacci (fib : nat -> nat) :=
  fib 0 = 0
  /\
  fib 1 = 1
  /\
  forall n'' : nat,
    fib (S (S n'')) = fib (S n'') + fib n''.

Fixpoint fibo (n : nat) : nat :=
  match n with
    | 0 => 0
    | S n' => match n' with
                | 0 => 1
                | S n'' => fibo n' + fibo n''
              end
  end.

Lemma unfold_fibo_base_case_0 :
  fibo 0 = 0.
Proof.
  unfold fibo.
  reflexivity.
Qed.

Lemma unfold_fibo_base_case_1 :
  fibo 1 = 1.
Proof.
  unfold fibo.
  reflexivity.
Qed.

Lemma unfold_fibo_induction_case :
  forall n'' : nat,
    fibo (S (S n'')) = fibo (S n'') + fibo n''.
Proof.
  intro n''.
  unfold fibo; fold fibo.
  reflexivity.
Qed.

(*
Compute exp (create 0 1 1 1) 2.
     = create 1 1 1 2
     : matrix_2x2

Compute exp (create 0 1 1 1) 3.
     = create 1 2 2 3
     : matrix_2x2

Compute exp (create 0 1 1 1) 4.
     = create 2 3 3 5
     : matrix_2x2

Compute exp (create 0 1 1 1) 5.
     = create 3 5 5 8
     : matrix_2x2
*)

Theorem Fibonacci_and_the_matrix_0_1_1_1 :
  forall n : nat, 
    exp (create 0 1 1 1) (S n) = create (fibo n) (fibo (S n)) (fibo (S n)) (fibo (S(S n))).
Proof.
  intro n.
  induction n as [ | n' IHn'].

  rewrite -> unfold_exp_ic.
  rewrite -> unfold_exp_bc.
  rewrite -> I_is_neutral_on_the_left.
  rewrite -> unfold_fibo_induction_case.
  rewrite -> unfold_fibo_base_case_0.
  rewrite -> unfold_fibo_base_case_1.
  rewrite -> plus_0_r.
  reflexivity.

  rewrite -> unfold_exp_ic.
  rewrite -> IHn'.
  rewrite -> unfold_times.
  rewrite ->2 mult_0_r.
  rewrite ->2 plus_0_l.
  rewrite ->3 mult_1_r.
  rewrite -> plus_comm.
  rewrite <- unfold_fibo_induction_case.
  rewrite -> plus_comm.
  rewrite -> unfold_fibo_induction_case.
  reflexivity.
Qed.


Definition specification_of_addition_matrices (addition : matrix_2x2 ->
                                                  matrix_2x2 ->
                                                  matrix_2x2) :=
  forall a b c d e f g h : nat,
      addition (create a b c d) (create e f g h) = 
create (a + e) (b + f) (c + g) (d + h).

Definition neutral_element :=
  create 0 0 0 0.

Theorem left_side :
  forall (mat : matrix_2x2),
    forall (addition : matrix_2x2 -> matrix_2x2 -> matrix_2x2),
      specification_of_addition_matrices addition ->
      addition neutral_element mat = mat. 
Proof.
  intros [a b c d] addition H_specification_of_addition_matrices.
  unfold specification_of_addition_matrices in H_specification_of_addition_matrices.
  unfold neutral_element.
  rewrite -> H_specification_of_addition_matrices.
  rewrite ->4 plus_0_l.
  reflexivity.
Qed.

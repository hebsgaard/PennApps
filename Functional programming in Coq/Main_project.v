(*Fie Hebsgaard - Pedersen, 20103511*)
(*Main project*)

Require Import Arith.

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

(*
Compute
  let foo p q := (fibo (S (p + q)),
                  fibo (S p) * fibo (S q) + fibo p * fibo q)
  in foo 5 3.
     = (34, 34)
     : nat * nat
*)

(*(a) prove d'Occagne's identity*)

Proposition about_fib_of_a_sum :
  forall fib : nat -> nat,
    specification_of_Fibonacci fib ->
    forall p q : nat,
      fib (S (p + q)) = fib (S p) * fib (S q) + fib p * fib q.
Proof.
  intro fib.
  intro specification.
  destruct specification as [H_fib_bc0 [H_fib_bc1 H_fib_ic]].

  intros p.
  induction p as [| p' IHp'].

  intro q.
  rewrite -> plus_0_l.
  rewrite -> H_fib_bc0. 
  rewrite -> H_fib_bc1.
  rewrite -> mult_0_l.
  rewrite -> plus_0_r.
  rewrite -> mult_1_l.
  reflexivity.

  intro q.
  rewrite -> H_fib_ic. 
  rewrite -> plus_Snm_nSm.
  rewrite -> IHp'.
  rewrite -> mult_plus_distr_r.
  rewrite -> H_fib_ic.
  rewrite -> mult_plus_distr_l.
  rewrite <-2 plus_assoc.
  rewrite -> (plus_comm (fib (S p') * fib q) (fib p' * fib (S q))).
  reflexivity.
Qed.

(*
"The whole of science is nothing more than a refinement of everyday thinking."
- Albert Einstein
*)

(*
Compute
  let foo n := (fibo (S n + S n),
               fibo (S n) * (fibo (S (S n)) + fibo n))
  in foo 4.
     = (55, 55)
     : nat * nat
*)

Proposition d_Occagne_s_identity :
  forall fib : nat -> nat,
    specification_of_Fibonacci fib ->
    forall n : nat,
      fib ((S n) + (S n)) = fib (S n) * (fib (S (S n)) + fib n).
Proof.
  intros fib.
  intros specification n.

  rewrite -> mult_plus_distr_l.
  rewrite -> mult_comm.
  rewrite <- (about_fib_of_a_sum fib specification _).
  rewrite -> plus_Sn_m.
  rewrite -> plus_comm.
  reflexivity.
Qed.


(*(b) prove Cassini's identity either for even numbers
    or for odd numbers,
    given the following square function:*)

Definition square (x : nat) : nat :=
  x * x.

Lemma unfold_square (x : nat):
  square x = x * x.

Proof.
  unfold square.
  reflexivity.
Qed.

(*
Compute
  let foo n := (square (fibo (S (n + n))),
              S ((fibo ((S n) + (S n))) * (fibo (n +  n))))
  in foo 3.
     = (169, 169)
     : nat * nat
*)

(*
"Believe you can and you're halfway there."
 - Theodore Roosevelt 
*)

Proposition Cassini_s_identity_for_even_numbers :
  forall fib : nat -> nat,
    specification_of_Fibonacci fib ->
    forall n : nat,
      square (fib (S (n + n))) =
      S ((fib ((S n) + (S n))) * (fib (n +  n))).
Proof.
  intros fib specification n.
  destruct specification as [H_fib_bc0 [H_fib_bc1 H_fib_ic]].
  induction n as [ | n' IHn']. 

  rewrite -> plus_0_r.
  rewrite -> H_fib_bc1.
  rewrite -> unfold_square.
  rewrite -> mult_1_r.
  rewrite -> H_fib_bc0.
  rewrite -> mult_0_r.
  reflexivity.

  rewrite -> unfold_square.
  rewrite -> plus_Sn_m at 2.
  rewrite -> H_fib_ic.
  rewrite -> mult_plus_distr_l.
  rewrite -> plus_Sn_m at 2.
  rewrite -> H_fib_ic.
  rewrite -> mult_plus_distr_r.
  rewrite <- unfold_square.
  rewrite <- (plus_n_Sm n' n').
  rewrite -> IHn'.
  rewrite -> plus_assoc.
  rewrite <- plus_n_Sm.
  rewrite -> (plus_n_Sm n' n').
  rewrite -> (mult_comm  (fib (S (n' + S n'))) (fib (n' + S n'))).
  rewrite <- mult_plus_distr_r.
  rewrite <- (plus_Sn_m n' (S n')).
  rewrite -> (mult_comm (fib (S n' + S n')) (fib (n' + n'))).
  rewrite <- mult_plus_distr_r.
  rewrite <- (plus_n_Sm n' n').
  rewrite <- plus_assoc.
  rewrite <- H_fib_ic.
  rewrite <- (plus_Sn_m n' n').
  rewrite -> (plus_n_Sm (S n') n').
  rewrite <- H_fib_ic.
  rewrite -> (plus_n_Sm (S n') (S n')).
  rewrite <- plus_Sn_m.
  reflexivity.
Qed.

(*
Compute
  let foo n := (S (square (fibo ((S n) + (S n)))), 
               (fibo (S ((S n) + (S n)))) * (fibo (S (n + n))))
  in foo 2.
     = (65, 65)
     : nat * nat
*)

(*"Bad times have a scientific value. These are occasions a good 
learner would not miss."
-Ralph Waldo Emerson *)

Proposition Cassini_s_identity_for_odd_numbers :
  forall fib : nat -> nat,
    specification_of_Fibonacci fib ->
    forall n : nat,
      S (square (fib ((S n) + (S n)))) =
      (fib (S ((S n) + (S n)))) * (fib (S (n + n))).
Proof.
  intros fib specification n.
  destruct specification as [H_fib_bc0 [H_fib_bc1 H_fib_ic]]. 
  induction n as [ | n' IHn']. 

  rewrite <- BinInt.ZL0.
  rewrite -> unfold_square.
  rewrite -> H_fib_ic.
  rewrite -> H_fib_bc0.
  rewrite -> H_fib_bc1.
  rewrite -> plus_0_r.
  rewrite -> mult_1_r.
  rewrite -> plus_0_r.
  rewrite -> H_fib_bc1.
  rewrite ->2 H_fib_ic.
  rewrite -> H_fib_bc0.
  rewrite -> H_fib_bc1.
  rewrite -> plus_0_r.
  rewrite <- BinInt.ZL0.
  rewrite -> mult_1_r.
  reflexivity.

  symmetry.
  rewrite -> plus_Sn_m at 1.
  rewrite -> H_fib_ic.
  rewrite -> mult_plus_distr_r.
  rewrite <- (plus_n_Sm (S n') n') at 2.
  rewrite -> H_fib_ic.
  rewrite -> mult_plus_distr_l.
  rewrite <- (plus_n_Sm (S n') (S n')) at 3.
  rewrite -> (plus_Sn_m n' n') at 2.
  rewrite <- IHn'.
  rewrite -> plus_assoc.
  rewrite <- plus_n_Sm.
  rewrite -> unfold_square.
  rewrite -> (plus_n_Sm (S n') n').
  rewrite <- plus_assoc.
  rewrite <- mult_plus_distr_r.
  rewrite <- plus_n_Sm.
  rewrite <- H_fib_ic.
  rewrite <- mult_plus_distr_l.
  rewrite <- H_fib_ic.
  rewrite <- unfold_square.
  rewrite -> plus_n_Sm.
  rewrite <- plus_Sn_m.
  reflexivity.
Qed.

(*
Compute
  let foo n := (square (fibo ((S n) + (S n))) + square (fibo (S (n + n))), 
               (fibo (S ((S n) + (S n)))) * (fibo (S (n + n))) + (fibo ((S n) + (S n))) * (fibo (n +  n)))
  in foo 3.
     = (610, 610)
     : nat * nat
*)

Proposition Composition_of_Cassini_s_identitys :
forall fib : nat -> nat,
    specification_of_Fibonacci fib ->
    forall n : nat,
      square (fib ((S n) + (S n))) + square (fib (S (n + n))) =
      (fib (S ((S n) + (S n)))) * (fib (S (n + n))) + (fib ((S n) + (S n))) * (fib (n +  n)).

Proof.
  intros fib specification n.
  rewrite <- (Cassini_s_identity_for_odd_numbers _ specification _).
  rewrite -> (Cassini_s_identity_for_even_numbers _ specification _).
  rewrite <- plus_Snm_nSm.
  reflexivity.
Qed.
(*
"Don't cry because it's over, smile because it happened."
- Dr. Seuss
*) 
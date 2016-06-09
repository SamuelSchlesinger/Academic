Theorem Composition :
    forall A B C : Type,
    forall f : A -> B,
    forall g : B -> C,
    exists h : A -> C, True.
Proof.
    intros A B C f g.
    exists (fun x => g (f x)).
    reflexivity.
Qed.

Definition Reduction {A C D B : Type} (f : A -> B) (g : C -> D) : Prop :=
    exists (encode : A -> C) (decode : D -> B),
    forall a : A, f a = decode (g (encode a)).

Theorem SelfReduction :
    forall (A A B B : Type) (f : A -> B),
    Reduction f f.
Proof.
    intros.
    hnf.
    exists (fun x => x).
    exists (fun x => x).
    tauto.
Qed.

Theorem ReductionComposes :
    forall (A C E F D B : Type) (f : A -> B) (g : C -> D) (h : E -> F),
    Reduction f g /\ Reduction g h -> Reduction f h.
Proof.
    intros A C E F D B f g h H.
    destruct H as (f_reduces_to_g, g_reduces_to_h).
    inversion f_reduces_to_g.
    inversion g_reduces_to_h.
    inversion H.
    inversion H0.
    simpl.
    hnf.
    exists (fun a => x0 (x a)).
    exists (fun y => x1 (x2 y)).
    intros a.
    specialize H1 with a.
    rewrite H1.
    specialize H2 with (x a).
    rewrite H2.
    reflexivity.
Qed.

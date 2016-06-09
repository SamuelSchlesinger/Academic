Definition associates { A : Type } (op : A -> A -> A) :=
    forall a b c : A, op a (op b c) = op (op a b) c.

Definition left_neutral { A : Type } (lid : A) (op : A -> A -> A) :=
    forall a : A, op lid a = a.

Definition right_neutral { A : Type } (rid : A) (op : A -> A -> A) :=
    forall a : A, op a rid = a.

Definition neutral { A : Type } (id : A) (op : A -> A -> A) :=
    left_neutral id op /\ right_neutral id op.

Definition inverse { A : Type } { op : A -> A -> A } { id : A }
    (neutral_id : neutral id op) (a : A) (ainv : A) :=
     op a ainv = id /\ op ainv a = id.

Definition inversesExist { A : Type } { op : A -> A -> A } { id : A } 
    (neutral_id : neutral id op) :=
    forall a : A, exists ainv : A, inverse neutral_id a ainv.

Definition magma ( A : Type ) ( op : A -> A -> A ) :=
    associates op.

Definition monoid ( A : Type ) ( op : A -> A -> A ) :=
    (exists id, neutral id op) /\ magma A op.

Definition mon_hom { A B : Type } { opA : A -> A -> A } { opB : B -> B -> B }
    (monoidA : monoid A opA) (hom : A -> B) (monoidB : monoid B opB) :=
    forall x y : A, hom (opA x y) = opB (hom x) (hom y).

Theorem mon_homs_compose : 
    forall (A B C : Type) (opA : A -> A -> A) (opB : B -> B -> B) (opC : C -> C -> C),
    forall (monoidA : monoid A opA) (monoidB : monoid B opB) (monoidC : monoid C opC),
    forall (homAB : A -> B) (homBC : B -> C),
    forall (mon_homAB : mon_hom monoidA homAB monoidB ) ( mon_homBC : mon_hom monoidB homBC monoidC ),
    exists (homAC : A -> C), mon_hom monoidA homAC monoidC.
Proof.
    intros.
    exists (fun a => homBC (homAB a)).
    unfold mon_hom.
    unfold mon_hom in mon_homAB.
    unfold mon_hom in mon_homBC.
    intros x y.
    specialize mon_homAB with x y.
    specialize mon_homBC with (homAB x) (homAB y).
    rewrite mon_homAB.
    rewrite mon_homBC.
    reflexivity.
Qed.

Theorem neutral_unique :
    forall (A : Type) (opA : A -> A -> A) (e g : A),
    neutral e opA /\ neutral g opA -> e = g.
Proof.
    intros.
    destruct H as (neutral_e, neutral_g).
    unfold neutral in *.
    destruct neutral_e as (ln_e, rn_e).
    destruct neutral_g as (ln_g, rn_g).
    unfold left_neutral in *.
    unfold right_neutral in *.
    specialize ln_e with g.
    specialize rn_g with e.
    rewrite <- ln_e.
    rewrite rn_g.
    reflexivity.
Qed.

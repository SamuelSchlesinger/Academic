Definition transitive { A : Type } (rel : A -> A -> bool) :=
    forall a b c, rel a b = true /\ rel b c = true -> rel a c = true.

Definition reflexive { A : Type } (rel : A -> A -> bool) :=
    forall a, rel a a = true.

Definition symmetric { A : Type } (rel : A -> A -> bool) := 
    forall a b, rel a b = true -> rel b a = true.

Definition prordering { A : Type } (lte : A -> A -> bool) :=
    reflexive lte /\ transitive lte.

Definition antisymmetric { A : Type } (rel : A -> A -> bool) :=
    forall a b, rel a b = true /\ rel b a = true -> a = b.

Definition pordering { A : Type } (lte : A -> A -> bool) :=
    prordering lte /\ antisymmetric lte.

Definition ordering { A : Type } (lte : A -> A -> bool) :=
    pordering lte /\ symmetric lte.

Inductive BST { A : Type } (lte : A -> A -> bool) { ord : ordering lte } :=
 | Branch : { n : nat } -> BST lte -> A -> BST lte -> BST lte
 | Leaf : BST lte 0.

Arguments Leaf {A} {lte} {ord}.
Arguments Branch {A} {lte} {ord} _ _ _.

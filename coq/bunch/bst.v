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

Inductive BST (A : Type) :=
 | Branch : BST A -> A -> BST A -> BST A
 | Leaf : BST A.

Arguments Branch {lte} _ _ _.
Arguments Leaf {A}.

Fixpoint insert { A : Type } { lte : A -> A -> bool } (ord : ordering lte) (bst : BST A) (a : A) :=
 match bst with
  | Branch l a' r => if andb (lte a a') (lte a' a) then Branch l a r
                else if lte a a' then insert ord l a 
                                 else insert ord r a 
  | Leaf => Branch Leaf a Leaf
 end.

Fixpoint member { A : Type } { lte : A -> A -> bool } (ord : ordering lte) (bst : BST A) (a : A) : bool :=
 match bst with
  | Branch l a' r => if andb (lte a a') (lte a' a) then true
                else if lte a a' then member ord l a
                                 else member ord r a
  | Leaf => false
 end.



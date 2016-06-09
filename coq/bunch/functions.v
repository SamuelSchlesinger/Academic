Definition compose { A B C : Type } (g : B -> C) (f : A -> B) :=
    fun a => g (f a).

Notation "f >> g" := (compose g f) (at level 70, right associativity).


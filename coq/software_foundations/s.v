Theorem plus_n_Sm : forall n m,
    S (plus n m) = plus n (S m).

Theorem plus_injective : forall n m,
    plus n n = plus m m -> n = m.
Admitted.

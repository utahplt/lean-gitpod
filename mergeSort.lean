-- TODO remove the `partial`s and convince Lean that mergeSort terminates

partial def merge [Ord A] (xs : List A) (ys : List A) : List A :=
  match xs, ys with
  | [], _ => ys
  | _, [] => xs
  | x'::xs', y'::ys' =>
    match Ord.compare x' y' with
    | .lt | .eq => x' :: merge xs' (y' :: ys')
    | .gt => y' :: merge (x'::xs') ys'

def splitList (lst : List A) : (List A × List A) :=
  match lst with
  | [] => ([], [])
  | x :: xs =>
    let (a, b) := splitList xs
    (x :: b, a)

partial def mergeSort [Ord A] (xs : List A) : List A :=
  if h : xs.length < 2 then
    match xs with
    | [] => []
    | [x] => [x]
  else
    let halves := splitList xs
    merge (mergeSort halves.fst) (mergeSort halves.snd)

def sorted (xs : List Nat) : Prop :=
  match xs with
  | [] => True
  | _ :: [] => True
  | x :: y :: xs => x <= y /\ sorted (y :: xs)

theorem mergeSort_sorts (xs : List Nat) :
  sorted (mergeSort xs) := by
  sorry


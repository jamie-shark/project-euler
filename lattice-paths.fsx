#load "assertions.fs"
#load "grid.fs"
open Assertions
open Grid

type 'a Tree =
    | Leaf
    | Branch of 'a * 'a Tree * 'a Tree

let flatten (tree:'a Tree) =
    let rec flattenBranches branchSoFar = function
        | Leaf                                   -> [List.rev branchSoFar]
        | Branch(value, left, right) -> [left;right]
                                                    |> List.collect (flattenBranches (value::branchSoFar))
    flattenBranches [] tree
    |> List.distinct

let getBranches coordinate branchLength =
    let rec branch coord = function
        | x when x = branchLength -> (coord, Leaf, Leaf)
        | x                       -> let right = getCoordinateOfDirection coord R
                                     let left = getCoordinateOfDirection coord D
                                     (coord, Branch(branch right (x+1)), Branch(branch left (x+1)))
    branch coordinate 0

getBranches (0, 0) 0 |> Is <| ((0, 0), Leaf, Leaf)
getBranches (0, 0) 1 |> Is <| ((0, 0),
                                Branch((1,  0), Leaf, Leaf),
                                Branch((0, -1), Leaf, Leaf))
getBranches (0, 0) 2 |> Is <| ((0, 0),
                                Branch((1,  0),
                                    Branch((2,  0), Leaf, Leaf),
                                    Branch((1, -1), Leaf, Leaf)),
                                Branch((0, -1),
                                    Branch((1, -1), Leaf, Leaf),
                                    Branch((0, -2), Leaf, Leaf)))

Branch(1, Leaf, Leaf) |> flatten |> Is [[1]]
Branch("a",
    Branch("b",
        Leaf,
        Branch("c", Leaf, Leaf)),
    Branch("d", Leaf, Leaf))
|> flatten |> Is [["a";"b"];["a";"b";"c"];["a";"d"]]

//latticePaths 1 1 |> Is [[R;D];[D;R]]

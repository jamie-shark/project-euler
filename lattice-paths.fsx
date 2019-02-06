#load "assertions.fs"
#load "grid.fs"
open Assertions
open Grid

type 'a tree =
    | EmptyTree
    | TreeNode of 'a * 'a tree * 'a tree

let treePaths (t:'a tree) =
    let rec subBranches branchSoFar = function
        | TreeNode(value, leftBranch, rightBranch) ->
            [leftBranch;rightBranch] |> List.collect (subBranches (value::branchSoFar))
        | EmptyTree ->
            [List.rev branchSoFar]
    subBranches [] t |> List.distinct

let getBranches coordinate branchLength =
    let rec branch coord = function
        | x when x = branchLength -> (coord, EmptyTree, EmptyTree)
        | x                       -> let right = getCoordinateOfDirection coord R
                                     let left = getCoordinateOfDirection coord D
                                     (coord, TreeNode(branch right (x+1)), TreeNode(branch left (x+1)))
    branch coordinate 0

getBranches (0, 0) 0 |> Is <| ((0, 0), EmptyTree, EmptyTree)
getBranches (0, 0) 1 |> Is <| ((0, 0),
                                TreeNode((1,  0), EmptyTree, EmptyTree),
                                TreeNode((0, -1), EmptyTree, EmptyTree))
getBranches (0, 0) 2 |> Is <| ((0, 0),
                                TreeNode((1,  0),
                                    TreeNode((2,  0), EmptyTree, EmptyTree),
                                    TreeNode((1, -1), EmptyTree, EmptyTree)),
                                TreeNode((0, -1),
                                    TreeNode((1, -1), EmptyTree, EmptyTree),
                                    TreeNode((0, -2), EmptyTree, EmptyTree)))

TreeNode(1, EmptyTree, EmptyTree) |> treePaths |> Is [[1]]
TreeNode("a",
    TreeNode("b",
        EmptyTree,
        TreeNode("c", EmptyTree, EmptyTree)),
    TreeNode("d", EmptyTree, EmptyTree))
|> treePaths |> Is [["a";"b"];["a";"b";"c"];["a";"d"]]

//latticePaths 1 1 |> Is [[R;D];[D;R]]

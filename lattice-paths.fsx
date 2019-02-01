#load "assertions.fs"
#load "grid.fs"
open Assertions
open Grid

type 'a tree =
    | EmptyTree
    | TreeNode of 'a * 'a tree * 'a tree

let getBranches coordinate branchLength =
    let rec branch coord = function
        | x when x = branchLength -> (coord, EmptyTree, EmptyTree)
        | x                       -> let right = getCoordinateOfDirection coord R
                                     let left = getCoordinateOfDirection coord D
                                     (coord, TreeNode(branch right (x+1)), TreeNode(branch left (x+1)))
    branch coordinate 0

getBranches (0, 0) 0 |> Is ((0, 0), EmptyTree, EmptyTree)
getBranches (0, 0) 1 |> Is ((0, 0),
                                TreeNode((1,  0), EmptyTree, EmptyTree),
                                TreeNode((0, -1), EmptyTree, EmptyTree))
getBranches (0, 0) 2 |> Is ((0, 0),
                                TreeNode((1,  0),
                                    TreeNode((2,  0), EmptyTree, EmptyTree),
                                    TreeNode((1, -1), EmptyTree, EmptyTree)),
                                TreeNode((0, -1),
                                    TreeNode((1, -1), EmptyTree, EmptyTree),
                                    TreeNode((0, -2), EmptyTree, EmptyTree)))

//latticePaths 1 1 |> Is [[R;D];[D;R]]

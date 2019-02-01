module Grid

type Direction = UL | U | UR | L | R | DL | D | DR

let getGrid width height =
    seq { for x in 0..width-1 do
            for y in 0..height-1 do
                yield (x, y)
    }

let getCoordinateOfDirection coord direction =
    match coord, direction with
    | (x, y), UL -> (x-1, y+1)
    | (x, y), U  -> (x  , y+1)
    | (x, y), UR -> (x+1, y+1)
    | (x, y), L  -> (x-1, y  )
    | (x, y), R  -> (x+1, y  )
    | (x, y), DL -> (x-1, y-1)
    | (x, y), D  -> (x  , y-1)
    | (x, y), DR -> (x+1, y-1)

let isPointInBounds width height = function
    | (x, y) when x >= 0 &&
                  x < width &&
                  y >= 0 &&
                  y < height -> true
    | _                      -> false



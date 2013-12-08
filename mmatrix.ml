let get_height matrix = Array.length matrix.(0)
let get_width matrix = Array.length matrix

let make width height init_val =
  let result = Array.make width (Array.make height init_val) in
        for i = 1 to width - 1 do
            result.(i) <- Array.make height init_val
        done;
        result

let resize matrix nw nh default =
  let w = get_width matrix and h = get_height matrix in
  let new_matrix = make nw nh default in
    for y = 0 to nh - 1 do
        for x = 0 to nw - 1 do
            new_matrix.(x+(nw-w)).(y+(nh-h)) <- matrix.(x).(y)
        done;
    done;
    new_matrix

let matrix2vector matrix =
  let w = get_width matrix and h = get_height matrix in
  let vect = Array.make (w * h) 0. in
    for y = 0 to h - 1 do
        for x = 0 to w - 1 do
            if(matrix.(x).(y)) then
                vect.(x + y*w) <- 1.
            else
                vect.(x + y*w) <- 0.
        done
    done;
    vect

let print matrix =
    for y = 0 to get_height matrix - 1 do
        for x = 0 to get_width matrix - 1 do
            Printf.printf "%c" (if matrix.(x).(y) then '1' else '0');
        done;
        Printf.printf "\n";
    done;

let () = print_endline "hello"

let x = 3

let my_fun x y = (int_of_string x) + y

let zz  = (1 + 1)
let zzz = 
        let xx = zz in

        let y = 3 + my_fun "3" 2  in

        let z = "" in

        let foo = my_fun "3" 2 in

        let x = print_endline in
        ignore (xx,y,z,x,foo)


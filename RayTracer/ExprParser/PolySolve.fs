module PolySolve

/// <summary>Solves a first degree polynomial given a and b</summary>
///<param name="str">a value of the polynomial to solve</param>
///<param name="str">b vlaue of the polynomial to solve</param>
let public solveFstDgrPoly a b = 
    Some((-b / a))


/// <summary>Solves a second degree polynomial given a, b, c and the descriminant D</summary>
///<param name="str">a value of the polynomial to solve</param>
///<param name="str">b vlaue of the polynomial to solve</param>
///<param name="str">c vlaue of the polynomial to solve</param>
///<param name="str">Descriminant D of the polynomial to solve.</param>
let public solveScnDgrPoly a b D = 
    if D >= 0. then
        let x1 = (-b + sqrt(D))/(2.*a)
        let x2 = (-b - sqrt(D))/(2.*a)

        if x1 > 0. && x2 > 0. then
            [x1;x2]
        else
            if x1 > 0. then [x1]
            elif x2 > 0. then [x2]
            else []
    else []

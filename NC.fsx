let countingOnes (l: int list)= List.length (List.filter (fun elem -> elem=1) l);;
let countingZeros (l: int list) = List.length (List.filter (fun elem -> elem=0) l);;
let createListGreaterThanOne (l: int list) = List.filter (fun elem -> elem >1) l;;
let createListLessThanZero (l: int list) = List.filter (fun elem -> elem <0) l;;
let isListGreaterThanOneEven (l: int list) = (List.length (createListGreaterThanOne l)) % 2= 0;;
let isListLessThanZeroEven (l: int list) = (List.length (createListLessThanZero l)) % 2= 0;;
let sortPositiveList l = List.sort(List.filter (fun elem -> elem > 0) l);;
let sortNegativeAndZeroList l =List.sortDescending(List.filter (fun elem -> elem < 1) l);;

(*succecively pair numbers in an even numbered list and multiply them*)
let rec succPairMultiplyEvenList = function
    | x0 :: x1 :: xs -> (x0*x1) :: succPairMultiplyEvenList(xs)
    | x0 :: xs -> x0*(List.head xs) :: succPairMultiplyEvenList(xs)
    | _              -> [];;
(*succesively pair numbers in an uneven numbered list and multiply them, while leaving the firs elements *)
let rec succPairMultiplyOddList = function
    | x0 :: x1 :: x2 :: xs -> (x1*x2) :: succPairMultiplyOddList(x2::xs)
    | x0 :: x1 :: xs       ->  x1*(List.head xs) :: succPairMultiplyOddList(xs)
    | _                    -> [];;

let sumPairMultiplyEvenList l = List.fold (fun s elem -> s + elem) 0 (succPairMultiplyEvenList l);;
let sumPairMultiplyOddList l = List.fold (fun s elem -> s + elem) 0 (succPairMultiplyOddList l);;

let greatestSumPositive l = 
    if (countingOnes l = 0) && (isListGreaterThanOneEven l = true) 
        then sumPairMultiplyEvenList l 
        else if (countingOnes l > 0) && (isListGreaterThanOneEven l =true ) 
            then countingOnes l + sumPairMultiplyEvenList (createListGreaterThanOne l) 
            else if  (countingOnes l > 0) && (isListGreaterThanOneEven l =false ) 
                then countingOnes l + l.[countingOnes l]+ sumPairMultiplyOddList (createListGreaterThanOne l)
    else sumPairMultiplyOddList l + l.[countingOnes l];;

let greatestSumNegative l =
    if (countingZeros l = 0) && (isListLessThanZeroEven l = true) 
        then sumPairMultiplyEvenList l
         else if (countingZeros l >0 ) && (isListLessThanZeroEven l = true) 
            then sumPairMultiplyEvenList (createListLessThanZero l)
            else if (countingZeros l > 0 ) && (isListLessThanZeroEven l = false) 
                then sumPairMultiplyOddList (createListLessThanZero l) + l.[countingZeros l] //this needs to be reviewievd
    else sumPairMultiplyOddList l + l.[countingZeros l];;

let greatestSum l= greatestSumPositive (sortPositiveList l) + greatestSumNegative (sortNegativeAndZeroList l);;

(*List of an uneven number of positive int greater than 1*)
let l1 =[2;6;3;7;4;8;17;9;5;6;13;4;13];;

(*List of an even number of positive int greater than 1*)
let l2 =[6;3;7;4;8;17;9;5;6;13;4;13];;

(*List of an uneven number of negative int less than 0*)
let l3=[-10;-5;-9;-5;-1;-6;-12;-20;-2];;

(*List of an even number of negative int less than 0*)
let l4=[-10;-9;-1;-5;-5;-6;-12;-20;-2;-1];;

(*List of an uneven number of positive int greater than 0*)
let l5=[2;6;1;7;4;1;8;17;9;5;6;13;4];;

(*List of an even number of positive int greater than 0*)
let l6=[2;6;4;1;8;1;17;9;5;6;13;4;13;1;6];;

(*List of number of uneven negative int less or equal to 0*)
let l7=[-10;-1;0;-5;-9;-1;-5;0;0;-5;-6;-12;-20;-2;-1];;

(*List of an even number of negative int less or equal to 0*)
let l8=[-10;-1;0;-5;-9;-1;-5;0;-5;-6;-12;-20;-2;-1;-1];;

let l9= l1 @ l4;;

let l10= l2 @ l8;;

let l11= l6 @ l7;;

let suml1= 464;;
let suml2= 462;;
let suml3 =369;;
let suml4 = 371;;
let suml5 = 385;;
let suml6 = 453;; 
let suml7= 388;;
let suml8= 387;;
let suml9= suml1 + suml4;;
let sum10= suml2 + suml8;;
let suml11= suml6 + suml7;;

let test1 = (suml1 = greatestSum l1);;

let test2 = suml2 = greatestSum l2;;

let test3= suml3 = greatestSum l3;;

let test4= suml4 = greatestSum l4;;

let test5= suml5 = greatestSum l5;;

let test6= suml6 = greatestSum l6;;

let test7= suml7 = greatestSum l7;;

let test8= suml8 = greatestSum l8;;

let test9= suml9 = greatestSum l9;;

let test10 = sum10 = greatestSum l10;;

let test11 = suml11 = greatestSum l11;;
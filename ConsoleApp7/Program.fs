open System


type BinaryTree<'T> =
    | Empty
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>


let rec smena tree =
    match tree with
    | Empty -> Empty
    | Node (value, left, right) ->
        let newValue = 
            if value < 0.0 then 0.0
            elif value > 0.0 then 1.0
            else value
        Node (newValue, smena left, smena right)


let rec foldTree f state tree =
    match tree with
    | Empty -> state
    | Node (value, left, right) ->
        let leftState = foldTree f state left
        let currentState = f leftState value
        foldTree f currentState right


let conDigit number digit =
    let rec checkDigits n =
        if n = 0 then false
        elif abs (n % 10) = digit then true
        else checkDigits (n / 10)
    
    if number = 0 then digit = 0
    else checkDigits (abs number)


let count state value digit =
    if conDigit value digit then state + 1 else state


let rec printTree tree =
    match tree with
    | Empty -> printf "Empty"
    | Node (value, left, right) ->
        printfn ""
        printf "Node(%A, " value
        printTree left
        printf ", "
        printTree right
        printf ")"


[<EntryPoint>]
let main argv =
    
    // ===== ЗАДАНИЕ 1 =====
    printfn "ЗАДАНИЕ 1: Замена отрицательных на 0, положительных на 1"
    

    let tree1 = 
        Node (5.0, 
            Node (-3.0, 
                Node (1.5, Empty, Empty), 
                Node (-2.0, Empty, Empty)),
            Node (0.0, 
                Node (7.0, Empty, Empty), 
                Node (-1.0, Empty, Empty)))
    
    printfn "Исходное дерево:"
    printTree tree1
    printfn ""
    
    let resultTree1 = smena tree1
    
    printfn "Результат после замены:"
    printTree resultTree1
    printfn "\n"
    
    
    // ===== ЗАДАНИЕ 2 =====
    printfn "ЗАДАНИЕ 2: Подсчет элементов, содержащих заданную цифру"
    

    let tree2 = 
        Node (123, 
            Node (45, 
                Node (78, Empty, Empty), 
                Node (90, Empty, Empty)),
            Node (12, 
                Node (345, Empty, Empty), 
                Node (67, Empty, Empty)))
    
    printfn "Дерево:"
    printTree tree2
    printfn ""
    
    printf "Введите цифру для поиска (0-9): "
    let digitInput = Console.ReadLine()
    
    match Int32.TryParse(digitInput) with
    | true, digit when digit >= 0 && digit <= 9 ->
        let count = foldTree (fun state value -> count state value digit) 0 tree2
        printfn "Количество элементов, содержащих цифру %d: %d" digit count
    | _ ->
        printfn "Ошибка: введите корректную цифру от 0 до 9"
    
    0
open System
open System.Collections.Generic
open System.IO

type Figure(owner: string, density: double) =
    member val Owner = owner
    member val Density = density
    override this.ToString() =
        sprintf "Figure: Density=%f, Owner=%s" this.Density this.Owner

type Sphere(radius: double, density: double, owner: string) =
    inherit Figure(owner, density)
    member val Radius = radius
    override this.ToString() =
        sprintf "Sphere: Radius=%f, Density=%f, Owner=%s" this.Radius this.Density this.Owner

type Parallelepiped(a: double, b: double, c: double, density: double, owner: string) =
    inherit Figure(owner, density)
    member val A = a
    member val B = b
    member val C = c
    override this.ToString() =
        sprintf "Parallelepiped: a=%f, b=%f, c=%f, Density=%f, Owner=%s" this.A this.B this.C this.Density this.Owner

type Cylinder(x: double, y: double, z: double, radius: double, height: double, density: double, owner: string) =
    inherit Figure(owner, density)
    member val X = x
    member val Y = y
    member val Z = z
    member val Radius = radius
    member val Height = height
    override this.ToString() =
        sprintf "Cylinder: (x=%f, y=%f, z=%f), Radius=%f, Height=%f, Density=%f, Owner=%s" this.X this.Y this.Z this.Radius this.Height this.Density this.Owner

type Node(value: Figure, next: Node option) =
    member val Value = value with get
    member val Next : Node option = next with get, set

type CircularLinkedList() =
    let mutable head = None
    let mutable tail = None
    let mutable count: int = 0

    member this.Add(figure: Figure) =
        let node = new Node(figure, None)
        match tail with
        | None ->
            head <- Some node
            tail <- Some node
            node.Next <- Some node
            count <- count + 1
        | Some t ->
            node.Next <- Some head.Value
            t.Next <- Some node
            tail <- Some node
            count <- count + 1

    member this.Print() =
        let mutable currentNode = head
        let writer = new System.IO.StreamWriter("output.txt" : string)
        Console.WriteLine("\nPrint")
        let rec printNode (node: Node option) =
            if node <> None then
                printfn "%A" node.Value.Value
                writer.WriteLine(node.Value.Value.ToString())
                if node <> tail then
                    printNode node.Value.Next       
        printNode currentNode        
        writer.Close()
    
    member this.Remove(parameters: string list) =  
        let mutable currentNode : Node option = head
        let mutable countList : int = count    
        while countList > 0 do
            for cur in currentNode.Value.Value.GetType().GetProperties() do   
                match cur.Name with   
                | value when value = parameters.[0] ->    
                    match parameters.[1] with   
                    | "==" | ">=" | "<=" | "<" | ">" | "!=" ->   
                        let condition = 
                            match parameters.[1] with
                            | "==" -> (cur.GetValue(currentNode.Value.Value).ToString()) = string(parameters.[2])
                            | ">=" -> (cur.GetValue(currentNode.Value.Value) :?> int) >= int(parameters.[2])
                            | "<=" -> (cur.GetValue(currentNode.Value.Value) :?> int) <= int(parameters.[2])
                            | "<" -> (cur.GetValue(currentNode.Value.Value) :?> int) < int(parameters.[2])
                            | ">" -> (cur.GetValue(currentNode.Value.Value) :?> int) > int(parameters.[2])
                            | "!=" -> (cur.GetValue(currentNode.Value.Value).ToString()) <> string(parameters.[2])
                            | _ -> false
                        if condition then   
                            this.RemoveNode currentNode
                            Console.WriteLine($"\n\"{currentNode.Value.Value}\" removed because:{parameters}");
                    | _ -> failwith $"Invalid condition: {parameters.[1]}" 
                | _ -> ()
            currentNode <- currentNode.Value.Next
            countList <- countList - 1

    member private this.RemoveNode (nodeToRemove: Node option) =  
        let mutable currentNode : Node option = head     
        let mutable prevNode : Node option = None  
        while currentNode <> None do  
            if (currentNode = nodeToRemove) then 
                match prevNode with  
                | None ->  
                    head <- currentNode.Value.Next  
                    tail.Value.Next <- head
                    count <- count - 1
                    currentNode <- None
                | Some p ->  
                    p.Next <- currentNode.Value.Next  
                    if currentNode = head then 
                        head <- p.Next 
                    if currentNode = tail then  
                        tail <- Some(p)
                    count <- count - 1
                    currentNode <- None
            if currentNode <> None then
                prevNode <- currentNode 
                currentNode <- currentNode.Value.Next
                if currentNode = head then
                    currentNode <- None

let list = new CircularLinkedList()

let readCommands (fileName: string) =
    let commands = ref []
    use file = new StreamReader(fileName)
    let lineNumber = ref 0
    while not file.EndOfStream do
        try
            let line = file.ReadLine()
            lineNumber := !lineNumber + 1
            let parts : string [] = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            match parts.[0].ToUpper() with
            | "ADD" -> commands := ("ADD", Array.tail parts |> Array.toList) :: !commands
            | "REM" -> commands := ("REM", Array.tail parts |> Array.toList) :: !commands
            | "PRINT" -> commands := ("PRINT", []) :: !commands
            | _ -> printfn "Invalid command while reading: %s" parts.[0]
        with
        | ex ->
            printfn "[%d] Error while reading: %s" !lineNumber ex.Message
    List.rev !commands




let tryParseDouble (s: string) =
    match Double.TryParse(s.Replace(".", ",")) with
    | (true, value) -> Some value
    | _ -> None

let add(parameters: string list) =
    match parameters.[0].ToUpper() with
    | "SPHERE" ->
        match tryParseDouble parameters.[1], tryParseDouble parameters.[2] with
        | Some radius, Some density -> 
            list.Add(new Sphere(radius, density, parameters.[3]))
        | _ -> failwith "Некорректные параметры для команды ADD Sphere"
    | "PARALLELEPIPED" ->
        match tryParseDouble parameters.[1], tryParseDouble parameters.[2], tryParseDouble parameters.[3], tryParseDouble parameters.[4] with
        | Some a, Some b, Some c, Some density -> 
            list.Add(new Parallelepiped(a, b, c, density, parameters.[5]))
        | _ -> failwith "Некорректные параметры для команды ADD Parallelepiped"
    | "CYLINDER" ->
        match tryParseDouble parameters.[1], tryParseDouble parameters.[2], tryParseDouble parameters.[3], tryParseDouble parameters.[4], tryParseDouble parameters.[5], tryParseDouble parameters.[6] with
        | Some x, Some y, Some z, Some radius, Some height, Some density -> 
            list.Add(new Cylinder(x, y, z, radius, height, density, parameters.[7]))
        | _ -> failwith "Некорректные параметры для команды ADD Cylinder"
    | _ -> failwith $"Некорректный тип фигуры: {parameters.[0]}"



let executeCommands (commands: (string * string list) list) =
    for cmd in commands do
        try
            match cmd with
            | ("ADD", item) -> add item
            | ("REM", item) -> list.Remove(item)
            | ("PRINT", _) -> list.Print()
            | _ -> ()
        with
        | ex ->
            printfn "\nОшибка при выполнении команды: %s" (String.concat " " (snd cmd))
            printfn "Детали исключения: %s" (ex.ToString())


let fileName = "data.txt"
let commands = readCommands fileName
executeCommands commands

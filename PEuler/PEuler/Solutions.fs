namespace PEuler

open System
open Microsoft.FSharp.Collections
open System.Collections.Generic

type NumbersInGrid = { row: int; column: int; number: int; }
    
type Euler() = 

    member this.IsPrime x = 
        let rec r y =             
            if y > int (System.Math.Sqrt(double x)) then true
            else if x % y = 0 then false
            else r(y + 1)

        if x < 2 then false
        else r 2   

    member this.FindPrimes x = 
        { 1 .. x }
        |> PSeq.filter(this.IsPrime)
                        
    member this.Problem10() =    
        this.FindPrimes(2000000)
        |> Seq.map(double)
        |> Seq.sum

    member this.Problem16() =        
        let rec twoToPowOf1000 iteration number =
            let doubleIt number =
                let rec doubleIt number doubled =
                    match number with
                    | [] -> doubled
                    | h::t -> doubleIt t ((h*2)::doubled)
                doubleIt number []

            let rec carryOverTens carried =
                let rec carryOverTens carried originalNumber =
                    match originalNumber with
                    | [] -> carried
                    | h::[] ->
                        if h < 10 then carryOverTens (h::carried) []
                        else 
                            let digit = h - 10
                            carryOverTens (digit::carried) (1::[])                        
                    | h::t::rest ->
                        if h < 10 then carryOverTens (h::carried) (t::rest) 
                            else 
                                let digit = h - 10
                                let plusCarried = t + 1
                                carryOverTens (digit::carried) (plusCarried::rest)    
                carryOverTens [] carried                   
            
            if iteration > 1000 then number
            else twoToPowOf1000 (iteration + 1) ((doubleIt number) |> carryOverTens)

        twoToPowOf1000 1 [1]
        |> List.sum
                         
    member this.Problem17() =
        let parsedNumbers = 
            "   08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
                49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
                81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
                52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
                22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
                24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
                32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
                67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
                24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
                21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
                78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
                16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
                86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
                19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
                04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
                88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
                04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
                20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
                20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
                01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48".Split('\n')
            |> Seq.mapi(fun row line -> 
                line.Trim().Split(' ') 
                |> Seq.mapi (fun column digits -> { row = row; column = column; number = (int digits); }))
            |> Seq.collect(fun i -> i)
            |> Seq.cache

        let largestProductNumberContributesTo num =
            let collectionOfContiguous4Numbers direction order =
                 parsedNumbers |> Seq.filter direction |> Seq.sortBy order |> Seq.windowed 4

            let sameRow = collectionOfContiguous4Numbers (fun x -> x.row = num.row && Math.Abs (num.column - x.column) <= 4) (fun x -> x.column)
            let sameColumn = collectionOfContiguous4Numbers (fun x -> x.column = num.column && Math.Abs(num.row - x.row) <= 4) (fun x -> x.row)
            let diagonalFromTopLeft = collectionOfContiguous4Numbers (fun x -> 
                let distance = num.row - x.row 
                distance = num.column - x.column && Math.Abs distance <= 4) (fun x -> x.column) 

            let diagonalFromBottomLeft = collectionOfContiguous4Numbers (fun x -> 
                let distance = num.row - x.row
                distance = x.column - num.column && Math.Abs distance <= 4) (fun x -> x.column) 

            [sameRow; sameColumn; diagonalFromBottomLeft; diagonalFromTopLeft] 
                |> Seq.collect(fun i -> i)
                |> Seq.map(fun x -> x |> Seq.fold(fun product i -> product * i.number) 1)
                |> Seq.max

        parsedNumbers
        |> Seq.map largestProductNumberContributesTo
        |> Seq.max       
        

    member this.Problem12() =
        
        let triangularNumbers = 
            Seq.unfold (fun (i, n) -> Some(n, (i+1, n+i))) (2, 1)

        let divisorsOf x = 
            let rec divisorsOf currentIndex divisors = 
                if currentIndex = 0 then divisors
                elif x % currentIndex = 0 then divisorsOf (currentIndex - 1) (divisors + 2)
                else divisorsOf (currentIndex - 1) divisors

            let sqrt = (int (System.Math.Sqrt(double x)))
            let initialDivisor =
                if sqrt * sqrt = x then -1
                else 0

            divisorsOf sqrt initialDivisor

        triangularNumbers |> Seq.find (fun x -> divisorsOf x > 500)


    member this.Problem13() =
        "   37107287533902102798797998220837590246510135740250
            46376937677490009712648124896970078050417018260538
            74324986199524741059474233309513058123726617309629
            91942213363574161572522430563301811072406154908250
            23067588207539346171171980310421047513778063246676
            89261670696623633820136378418383684178734361726757
            28112879812849979408065481931592621691275889832738
            44274228917432520321923589422876796487670272189318
            47451445736001306439091167216856844588711603153276
            70386486105843025439939619828917593665686757934951
            62176457141856560629502157223196586755079324193331
            64906352462741904929101432445813822663347944758178
            92575867718337217661963751590579239728245598838407
            58203565325359399008402633568948830189458628227828
            80181199384826282014278194139940567587151170094390
            35398664372827112653829987240784473053190104293586
            86515506006295864861532075273371959191420517255829
            71693888707715466499115593487603532921714970056938
            54370070576826684624621495650076471787294438377604
            53282654108756828443191190634694037855217779295145
            36123272525000296071075082563815656710885258350721
            45876576172410976447339110607218265236877223636045
            17423706905851860660448207621209813287860733969412
            81142660418086830619328460811191061556940512689692
            51934325451728388641918047049293215058642563049483
            62467221648435076201727918039944693004732956340691
            15732444386908125794514089057706229429197107928209
            55037687525678773091862540744969844508330393682126
            18336384825330154686196124348767681297534375946515
            80386287592878490201521685554828717201219257766954
            78182833757993103614740356856449095527097864797581
            16726320100436897842553539920931837441497806860984
            48403098129077791799088218795327364475675590848030
            87086987551392711854517078544161852424320693150332
            59959406895756536782107074926966537676326235447210
            69793950679652694742597709739166693763042633987085
            41052684708299085211399427365734116182760315001271
            65378607361501080857009149939512557028198746004375
            35829035317434717326932123578154982629742552737307
            94953759765105305946966067683156574377167401875275
            88902802571733229619176668713819931811048770190271
            25267680276078003013678680992525463401061632866526
            36270218540497705585629946580636237993140746255962
            24074486908231174977792365466257246923322810917141
            91430288197103288597806669760892938638285025333403
            34413065578016127815921815005561868836468420090470
            23053081172816430487623791969842487255036638784583
            11487696932154902810424020138335124462181441773470
            63783299490636259666498587618221225225512486764533
            67720186971698544312419572409913959008952310058822
            95548255300263520781532296796249481641953868218774
            76085327132285723110424803456124867697064507995236
            37774242535411291684276865538926205024910326572967
            23701913275725675285653248258265463092207058596522
            29798860272258331913126375147341994889534765745501
            18495701454879288984856827726077713721403798879715
            38298203783031473527721580348144513491373226651381
            34829543829199918180278916522431027392251122869539
            40957953066405232632538044100059654939159879593635
            29746152185502371307642255121183693803580388584903
            41698116222072977186158236678424689157993532961922
            62467957194401269043877107275048102390895523597457
            23189706772547915061505504953922979530901129967519
            86188088225875314529584099251203829009407770775672
            11306739708304724483816533873502340845647058077308
            82959174767140363198008187129011875491310547126581
            97623331044818386269515456334926366572897563400500
            42846280183517070527831839425882145521227251250327
            55121603546981200581762165212827652751691296897789
            32238195734329339946437501907836945765883352399886
            75506164965184775180738168837861091527357929701337
            62177842752192623401942399639168044983993173312731
            32924185707147349566916674687634660915035914677504
            99518671430235219628894890102423325116913619626622
            73267460800591547471830798392868535206946944540724
            76841822524674417161514036427982273348055556214818
            97142617910342598647204516893989422179826088076852
            87783646182799346313767754307809363333018982642090
            10848802521674670883215120185883543223812876952786
            71329612474782464538636993009049310363619763878039
            62184073572399794223406235393808339651327408011116
            66627891981488087797941876876144230030984490851411
            60661826293682836764744779239180335110989069790714
            85786944089552990653640447425576083659976645795096
            66024396409905389607120198219976047599490197230297
            64913982680032973156037120041377903785566085089252
            16730939319872750275468906903707539413042652315011
            94809377245048795150954100921645863754710598436791
            78639167021187492431995700641917969777599028300699
            15368713711936614952811305876380278410754449733078
            40789923115535562561142322423255033685442488917353
            44889911501440648020369068063960672322193204149535
            41503128880339536053299340368006977710650566631954
            81234880673210146739058568557934581403627822703280
            82616570773948327592232845941706525094512325230608
            22918802058777319719839450180888072429661980811197
            77158542502016545090413245809786882778948721859617
            72107838435069186155435662884062257473692284509516
            20849603980134001723930671666823555245252804609722
            53503534226472524250874054075591789781264330331690".Trim().Split('\n') 
            |> Seq.map BigNum.Parse
            |> Seq.sum
            |> string
            |> Seq.take 10
            |> System.String.Concat
            |> double
                
    member this.Problem14() =            
        let is_odd x =
            x % 2L = 1L
 
        let rec collatz_sequence n (partial:int64 list) (cache: IDictionary<int64, int64 list>) =
            if cache.ContainsKey(n) then
                let result = partial @ cache.[n]
                let sequence_starter = List.head result
                if not(cache.ContainsKey(sequence_starter)) then
                    cache.Add(sequence_starter, result) |> ignore
                result
            else
                match n with
                | 1L ->
                    let result = partial @ [1L]
                    let sequence_starter = List.head result
                    cache.Add(sequence_starter, result) |> ignore
                    result
                | n when is_odd n -> collatz_sequence ((3L*n) + 1L) (partial @ [n]) cache
                | _ -> collatz_sequence (n/2L) (partial @ [n]) cache
 
        let sequence_cache = new Dictionary<int64, int64 list>()
 
        let calculate_sequence n =
            collatz_sequence n [] sequence_cache
 
        let max_sequence_lengths (max:int64) =
            let all_sequences = [for i in 1L..max -> calculate_sequence i] 
            let lengths = List.map List.length all_sequences
            let max = List.max lengths
            let max_index = List.findIndex (fun (x:int64 list) -> x.Length = max) all_sequences
            max_index + 1

        max_sequence_lengths 1000000L


    member this.Problem15() =
        let cache = Dictionary<(Int64 * Int64), Int64>()
        let rec pascalTriangle row column = 
            if cache.ContainsKey(row,column) then cache.[(row,column)]
            elif column = 0L || row = 0L then 1L
            else 
                let rows = pascalTriangle (row - 1L) column
                let cols = pascalTriangle row (column - 1L)
                let sum = rows + cols
                cache.[(row,column)] <- sum
                sum
                
        pascalTriangle 20L 20L         
        
    member this.Problem19() =
        DateTime(1901,1,1) 
        |> Seq.unfold(fun d -> if d.Year > 2000 then None else Some(d, d.AddMonths(1)))
        |> Seq.filter(fun d -> d.DayOfWeek = DayOfWeek.Sunday)
        |> Seq.length
        
    member this.Problem20() =
        let factorial (n:Microsoft.FSharp.Math.BigNum) = 
            let rec factorial (acc:BigNum) (n:BigNum) =
                if n = BigNum.One then acc
                else factorial (acc * n) (n - BigNum.One)
            factorial BigNum.One n

        string (factorial (BigNum.FromInt(100)))
        |> Seq.map string
        |> Seq.map int
        |> Seq.sum

    member this.Problem21() =

        let sumDivisorsOf x =
            let divisorsOfX() = 
                let rec loop divisors i =
                    if i = 1 then 1::divisors
                    elif x % i = 0 then loop (i::(x/i)::divisors) (i-1)
                    else loop divisors (i-1)
                
                let initialValue = int (System.Math.Sqrt(double x))
                loop [] initialValue
            
            let calculation = divisorsOfX() |> List.sum
            calculation

        let result = 
            let limit = 10000
            let rec loop i amicables=
                if i = limit then amicables
                else
                    let iDivisors = sumDivisorsOf i
                    let b = Seq.tryFind(fun x -> x <> i && x = iDivisors && (sumDivisorsOf(x)) = i ) { 1 .. limit }
                    match b with
                    | None -> loop (i + 1) amicables
                    | Some(amicable) -> loop (i + 1) (i::amicables)
                                
            loop 1 [] |> Seq.sum

        result

    member this.Problem25() =
        
        let fibonacci = Seq.unfold(fun (a, b) -> Some(b, (b, b + a)) ) (BigNum.Zero, BigNum.One)

        (fibonacci |> Seq.findIndex(fun x -> x.ToString().Length = 1000)) + 1
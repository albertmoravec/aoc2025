#r "nuget: FParsec"

open System.IO
open FParsec

type LightConfiguration = list<bool>
type Switch = list<int>

type MachineConfig =
    { ExpectedLights: LightConfiguration
      Switches: list<Switch>
      ExpectedJoltage: list<int> }

let input = File.ReadAllLines "./10/demo.txt" |> Array.toList

// Parsers START

let betweenStrings s1 s2 p = between (pstring s1) (pstring s2) p
let delimitedNumbers delimiter = sepBy1 pint32 (pstring delimiter)

let pLightState = (pstring "." >>% false) <|> (pstring "#" >>% true)
let pExpectedLights = betweenStrings "[" "]" (many1 pLightState)

let pSwitch = betweenStrings "(" ")" (delimitedNumbers ",")

let pSwitches =
    many1Till (pSwitch .>> spaces1) (followedBy (spaces >>. pstring "{"))

let pExpectedJoltage = betweenStrings "{" "}" (delimitedNumbers ",")

let pMachineConfig =
    pipe3
        pExpectedLights
        (spaces >>. pSwitches)
        (spaces >>. pExpectedJoltage)
        (fun expectedLights switches expectedJoltage ->
            { ExpectedLights = expectedLights
              Switches = switches
              ExpectedJoltage = expectedJoltage })

// Parsers END

let machineConfigs =
    input
    |> List.map (run pMachineConfig)
    |> List.map (fun x ->
        match x with
        | Success(result, _, _) -> result
        | Failure(_, _, _) -> failwith "Invalid machine configuration")

let pressSwitch lightConfiguration (switch: Switch) =
    switch
    |> List.fold
        (fun config position ->
            let currentState = List.item position config
            List.updateAt position (not currentState) config)
        lightConfiguration

let nextStates lightConfiguration switches =
    switches |> List.map (pressSwitch lightConfiguration)

let initialConfig (machineConfig: MachineConfig) =
    List.init (List.length machineConfig.ExpectedLights) (fun _ -> false)

let rec bfsExpectedState expectedState (switches: list<Switch>) queue visitedConfigs =
    match queue with
    | [] -> None
    | (config, depth) :: _ when config = expectedState -> Some(config, depth)
    | (config, depth) :: tail when Set.contains config visitedConfigs ->
        bfsExpectedState expectedState switches tail visitedConfigs
    | (config, depth) :: tail ->
        let nextStates = nextStates config switches |> List.map (fun x -> x, depth + 1)
        bfsExpectedState expectedState switches (tail @ nextStates) (Set.add config visitedConfigs)

let result =
    machineConfigs
    |> List.mapi (fun index x ->
        printfn "Processing item %d" index
        bfsExpectedState x.ExpectedLights x.Switches [ initialConfig x, 0 ] Set.empty)
    |> List.map (fun x ->
        match x with
        | Some(_, depth) -> depth
        | _ -> failwith "Could not find valid configuration")
    |> List.sum

printfn "%A" result

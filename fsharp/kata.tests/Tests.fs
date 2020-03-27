module Tests

open System
open Xunit
open Swensen.Unquote
open FsCheck.Xunit
open FsCheck

type Dice =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six

type Roll = Dice * Dice * Dice * Dice * Dice

let diceValue dice = match dice with
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6

let toDiceList roll =
    let dice1, dice2, dice3, dice4, dice5 = roll
    [dice1; dice2; dice3; dice4; dice5]

let chance roll =
    roll |> toDiceList |> List.sumBy diceValue

[<Property>]
let ``Chance roll is >= 5 and <= 30`` (roll:Roll) =
    let result = roll |> chance
    result >= 5 && result <= 30

[<Property>]
let ``Chance roll is multiple of 5 when all dices are equals`` (dice:Dice) =
    let result = (dice, dice, dice, dice, dice) |> chance
    result % 5 = 0

[<Fact>]
let ``Chance result should be the sum of dices`` () =
    chance (One, Three, Four, Six, Two) = 16

let yahtzee roll =
    match roll |> toDiceList |> List.distinct |> List.length with
    | 1 -> 50
    | _ -> 0

[<Property>]
let ``Yahtzee returns 50`` (dice:Dice) =
    yahtzee (dice, dice, dice, dice, dice) = 50

[<Property>]
let ``Yahtzee returns 0`` (dice1:Dice, dice2:Dice) =
    dice1 <> dice2 ==> lazy
    yahtzee (dice1, dice1, dice1, dice2, dice2) = 0

let fullHouse roll = 0
    (* WIP
    let groupedDices =
        roll
        |> toDiceList
        |> List.groupBy id
    groupedDices |> List.length = 2
    && groupedDices |> List.head |> snd |> List.length = 3*)

[<Property>]
let ``Full House returns 25`` (dice1:Dice) (dice2:Dice) =
    dice1 <> dice2 ==> lazy
    fullHouse (dice1, dice1, dice1, dice2, dice2) = 25

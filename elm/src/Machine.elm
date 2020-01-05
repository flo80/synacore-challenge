module Machine exposing (Machine, Pointer(..), State(..), Value(..), disassemble, fetchInstruction, getOutput, newMachine, showInstruction, step)

import Array
import Bitwise
import Char exposing (fromCode, isAlphaNum)
import String exposing (fromInt)



-- constants


modulo =
    32768


memorysize =
    modulo - 1


registersize =
    8


type alias Machine =
    { memory : Array.Array Int
    , registers : Array.Array Value
    , stack : List Value
    , ip : Pointer
    , state : State
    , input : List Int
    , output : List Char
    }


type Register
    = Register Rnr


type Rnr
    = R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | InvalidRegister


getRegisterIndex : Rnr -> Int
getRegisterIndex r =
    case r of
        R0 ->
            0

        R1 ->
            1

        R2 ->
            2

        R3 ->
            3

        R4 ->
            4

        R5 ->
            5

        R6 ->
            6

        R7 ->
            7

        InvalidRegister ->
            -1


getRegisterNr : Int -> Rnr
getRegisterNr address =
    case address of
        32768 ->
            R0

        32769 ->
            R1

        32770 ->
            R2

        32771 ->
            R3

        32772 ->
            R4

        32773 ->
            R5

        32774 ->
            R6

        32775 ->
            R7

        _ ->
            InvalidRegister


type Value
    = Value Int


type Pointer
    = Pointer Int


type Argument
    = V Value
    | R Register
    | InvalidArgument Int


type State
    = Halted
    | Running
    | WaitingForInput
    | Crashed String


type Instruction
    = HALT
    | SET Register Value
    | PUSH Value
    | POP Register
    | EQ Register Value Value
    | GT Register Value Value
    | JMP Pointer
    | JT Value Pointer
    | JF Value Pointer
    | ADD Register Value Value
    | MULT Register Value Value
    | MOD Register Value Value
    | AND Register Value Value
    | OR Register Value Value
    | NOT Register Value
    | RMEM Register Pointer
    | WMEM Pointer Value
    | CALL Pointer
    | RET
    | OUT Value
    | NOOP
    | INVALID Int


newMachine : List Int -> Machine
newMachine mem =
    let
        -- TODO: check length of list > memory size
        memoryTemp =
            Array.fromList mem

        fillerLength =
            memorysize + Array.length memoryTemp

        memory =
            if fillerLength > 0 then
                Array.append memoryTemp (Array.repeat fillerLength 0)

            else
                memoryTemp

        registers =
            Array.repeat registersize (Value 0)
    in
    Machine memory registers [] (Pointer 0) Running [] []



-- Instructions to interact with machine


getRegisterValue : Register -> Machine -> Maybe Value
getRegisterValue (Register reg) machine =
    let
        r =
            getRegisterIndex reg
    in
    Array.get r machine.registers


setRegisterValue : Register -> Value -> Machine -> Machine
setRegisterValue (Register reg) (Value value) machine =
    let
        r =
            getRegisterIndex reg

        v =
            Value (modBy modulo value)
    in
    case reg of
        InvalidRegister ->
            { machine | state = Crashed "Invalid register" }

        _ ->
            { machine | registers = Array.set r v machine.registers }


setIp : Pointer -> Machine -> Machine
setIp (Pointer newip) machine =
    if newip >= memorysize || newip < 0 then
        { machine | state = Crashed "IP outside memory" }

    else
        { machine | ip = Pointer newip }


getMemory : Pointer -> Machine -> Value
getMemory (Pointer source) machine =
    Value (Maybe.withDefault 0 <| Array.get source machine.memory)


setMemory : Pointer -> Value -> Machine -> Machine
setMemory (Pointer target) (Value value) machine =
    let
        v =
            modBy modulo value
    in
    if target >= memorysize || target < 0 then
        { machine | state = Crashed "memory access outside bounds" }

    else
        { machine | memory = Array.set target v machine.memory }


getArgument : Int -> Argument
getArgument value =
    if value < modulo then
        V (Value value)

    else
        let
            r =
                getRegisterNr value
        in
        case r of
            InvalidRegister ->
                InvalidArgument value

            _ ->
                R (Register r)


getValue : Machine -> Argument -> Maybe Value
getValue machine arg =
    case arg of
        V value ->
            Just value

        R register ->
            getRegisterValue register machine

        InvalidArgument _ ->
            Nothing


fetchInstruction : Machine -> ( Instruction, Pointer )
fetchInstruction machine =
    let
        memory =
            machine.memory

        (Pointer ip) =
            machine.ip

        opcode =
            Array.get ip memory

        gp : Value -> Pointer
        gp (Value v) =
            Pointer v

        gv : Int -> Value
        gv pos =
            Maybe.withDefault (Value 0) <|
                getValue machine <|
                    ga pos

        ga : Int -> Argument
        ga pos =
            let
                val =
                    Array.get pos memory
            in
            case val of
                Just x ->
                    getArgument x

                Nothing ->
                    InvalidArgument pos

        gv1 =
            gv (ip + 1)

        gv2 =
            gv (ip + 2)

        gv3 =
            gv (ip + 3)

        gr : Int -> Register
        gr pos =
            let
                r_ =
                    Array.get pos memory
            in
            case r_ of
                Nothing ->
                    Register InvalidRegister

                Just x ->
                    Register (getRegisterNr x)

        gr1 =
            gr (ip + 1)
    in
    case opcode of
        Nothing ->
            ( INVALID 0, Pointer ip )

        Just 0 ->
            ( HALT, Pointer ip )

        Just 1 ->
            ( SET gr1 gv2, Pointer (ip + 3) )

        Just 2 ->
            ( PUSH gv1, Pointer (ip + 2) )

        Just 3 ->
            ( POP gr1, Pointer (ip + 2) )

        Just 4 ->
            ( EQ gr1 gv2 gv3, Pointer (ip + 4) )

        Just 5 ->
            ( GT gr1 gv2 gv3, Pointer (ip + 4) )

        Just 6 ->
            ( JMP (gp gv1), Pointer (ip + 2) )

        Just 7 ->
            ( JT gv1 (gp gv2), Pointer (ip + 3) )

        Just 8 ->
            ( JF gv1 (gp gv2), Pointer (ip + 3) )

        Just 9 ->
            ( ADD gr1 gv2 gv3, Pointer (ip + 4) )

        Just 10 ->
            ( MULT gr1 gv2 gv3, Pointer (ip + 4) )

        Just 11 ->
            ( MOD gr1 gv2 gv3, Pointer (ip + 4) )

        Just 12 ->
            ( AND gr1 gv2 gv3, Pointer (ip + 4) )

        Just 13 ->
            ( OR gr1 gv2 gv3, Pointer (ip + 4) )

        Just 14 ->
            ( NOT gr1 gv2, Pointer (ip + 3) )

        Just 15 ->
            ( RMEM gr1 (gp gv2), Pointer (ip + 3) )

        Just 16 ->
            ( WMEM (gp gv1) gv2, Pointer (ip + 3) )

        Just 17 ->
            ( CALL (gp gv1), Pointer (ip + 2) )

        Just 18 ->
            ( RET, Pointer (ip + 1) )

        Just 19 ->
            ( OUT gv1, Pointer (ip + 2) )

        Just 21 ->
            ( NOOP, Pointer (ip + 1) )

        Just x ->
            ( INVALID x, Pointer (ip + 1) )


step : Machine -> Machine
step machine =
    let
        ( instruction, newIp ) =
            fetchInstruction machine

        sv target value =
            machine |> setIp newIp |> setRegisterValue target value
    in
    case instruction of
        HALT ->
            { machine | state = Halted }

        NOOP ->
            setIp newIp machine

        INVALID x ->
            let
                (Pointer ip) =
                    machine.ip
            in
            { machine | state = Crashed ("Invalid Instruction " ++ fromInt x ++ " at IP " ++ fromInt ip) }

        OUT (Value val) ->
            setIp newIp { machine | output = machine.output ++ [ fromCode val ] }

        JMP target ->
            setIp target machine

        JT (Value cmp) target ->
            machine
                |> setIp
                    (if cmp /= 0 then
                        target

                     else
                        newIp
                    )

        JF (Value cmp) target ->
            machine
                |> setIp
                    (if cmp == 0 then
                        target

                     else
                        newIp
                    )

        PUSH val ->
            setIp newIp { machine | stack = val :: machine.stack }

        POP target ->
            case List.head machine.stack of
                Nothing ->
                    { machine | state = Crashed "Empty Stack" }

                Just val ->
                    setIp newIp { machine | stack = Maybe.withDefault [] <| List.tail machine.stack }
                        |> setRegisterValue target val

        SET target value ->
            sv target value

        EQ target (Value cmpa) (Value cmpb) ->
            let
                value =
                    if cmpa == cmpb then
                        Value 1

                    else
                        Value 0
            in
            sv target value

        GT target (Value cmpa) (Value cmpb) ->
            let
                value =
                    if cmpa > cmpb then
                        Value 1

                    else
                        Value 0
            in
            sv target value

        ADD target (Value opa) (Value opb) ->
            sv target <| Value (opa + opb)

        MULT target (Value opa) (Value opb) ->
            sv target <| Value (opa * opb)

        MOD target (Value opa) (Value opb) ->
            sv target <| Value (modBy opb opa)

        AND target (Value opa) (Value opb) ->
            sv target <| Value (Bitwise.and opa opb)

        OR target (Value opa) (Value opb) ->
            sv target <| Value (Bitwise.or opa opb)

        NOT target (Value opa) ->
            sv target <| Value (Bitwise.and memorysize <| Bitwise.complement opa)

        RMEM target address ->
            sv target <| getMemory address machine

        WMEM target value ->
            machine |> setIp newIp |> setMemory target value

        CALL target ->
            let
                (Pointer ipval) =
                    newIp
            in
            setIp target { machine | stack = Value ipval :: machine.stack }

        RET ->
            case List.head machine.stack of
                Nothing ->
                    { machine | state = Crashed "Empty Stack in ret" }

                Just (Value val) ->
                    { machine | ip = Pointer val, stack = Maybe.withDefault [] <| List.tail machine.stack }


getOutput : Machine -> String
getOutput machine =
    String.fromList machine.output


showInstruction : Instruction -> String
showInstruction instr =
    let
        sp (Pointer p) =
            "->" ++ fromInt p ++ " "

        sr (Register r) =
            "R" ++ fromInt (getRegisterIndex r) ++ " "

        sv (Value x) =
            fromInt x
                ++ (if isAlphaNum (fromCode x) then
                        "(" ++ String.fromChar (fromCode x) ++ ") "

                    else
                        " "
                   )
    in
    case instr of
        HALT ->
            "HALT "

        SET a b ->
            "SET " ++ sr a ++ sv b

        PUSH a ->
            "PUSH " ++ sv a

        POP a ->
            "POP " ++ sr a

        EQ a b c ->
            "EQ " ++ sr a ++ sv b ++ sv c

        GT a b c ->
            "GT " ++ sr a ++ sv b ++ sv c

        JMP a ->
            "JMP " ++ sp a

        JT a b ->
            "JT " ++ sv a ++ sp b

        JF a b ->
            "JF " ++ sv a ++ sp b

        ADD a b c ->
            "ADD " ++ sr a ++ sv b ++ sv c

        MULT a b c ->
            "MULT " ++ sr a ++ sv b ++ sv c

        MOD a b c ->
            "MOD " ++ sr a ++ sv b ++ sv c

        AND a b c ->
            "AND " ++ sr a ++ sv b ++ sv c

        OR a b c ->
            "OR " ++ sr a ++ sv b ++ sv c

        NOT a b ->
            "NOT " ++ sr a ++ sv b

        RMEM a b ->
            "RMEM " ++ sr a ++ sp b

        WMEM a b ->
            "WMEM " ++ sp a ++ sv b

        CALL a ->
            "CALL " ++ sp a

        RET ->
            "RET "

        OUT a ->
            "OUT " ++ sv a

        NOOP ->
            "NOOP "

        INVALID x ->
            "INVALID " ++ sv (Value x)


disassemble : Array.Array Int -> Int -> ( String, Int )
disassemble memory pointer =
    let
        opcode =
            gt pointer

        gt x =
            Maybe.withDefault -1 <| Array.get x memory

        a1 =
            a <| gt (pointer + 1)

        a2 =
            a1 ++ (a <| gt (pointer + 2))

        a3 =
            a2 ++ (a <| gt (pointer + 3))

        a pos =
            let
                arg =
                    getArgument pos
            in
            case arg of
                V (Value x) ->
                    fromInt x
                        ++ (if isAlphaNum (fromCode x) then
                                "(" ++ String.fromChar (fromCode x) ++ ") "

                            else
                                " "
                           )

                R (Register r) ->
                    "R" ++ fromInt (getRegisterIndex r) ++ " "

                InvalidArgument i ->
                    "!" ++ fromInt i ++ " "
    in
    case opcode of
        0 ->
            ( "HALT ", pointer + 1 )

        1 ->
            ( "SET  " ++ a2, pointer + 3 )

        2 ->
            ( "PUSH " ++ a1, pointer + 2 )

        3 ->
            ( "POP  " ++ a1, pointer + 2 )

        4 ->
            ( "EQ   " ++ a3, pointer + 4 )

        5 ->
            ( "GT   " ++ a3, pointer + 4 )

        6 ->
            ( "JMP  " ++ a1, pointer + 2 )

        7 ->
            ( "JT   " ++ a2, pointer + 3 )

        8 ->
            ( "JF   " ++ a2, pointer + 3 )

        9 ->
            ( "ADD  " ++ a3, pointer + 4 )

        10 ->
            ( "MULT " ++ a3, pointer + 4 )

        11 ->
            ( "MOD  " ++ a3, pointer + 4 )

        12 ->
            ( "AND  " ++ a3, pointer + 4 )

        13 ->
            ( "OR   " ++ a3, pointer + 4 )

        14 ->
            ( "NOT  " ++ a2, pointer + 3 )

        15 ->
            ( "RMEM " ++ a2, pointer + 3 )

        16 ->
            ( "WMEM " ++ a2, pointer + 3 )

        17 ->
            ( "CALL " ++ a1, pointer + 2 )

        18 ->
            ( "RET  ", pointer + 1 )

        19 ->
            ( "OUT  " ++ a1, pointer + 2 )

        20 ->
            ( "IN   " ++ a1, pointer + 2 )

        21 ->
            ( "NOOP ", pointer + 1 )

        x ->
            ( "INV! " ++ fromInt x, pointer + 1 )

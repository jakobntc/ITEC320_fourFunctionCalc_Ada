with Ada.Characters.Handling; use Ada.Characters.Handling;
with ada.integer_text_io; use ada.integer_text_io;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with stackpkg;

-- Takes in user input or file input that is an equation without exponents,
-- then evaluates the equation using order of operations and then left to
-- right presedents.
procedure opcalcs is
    type charType is (Num, Sym);

    -- My attempt at variant records. i was going to try and implement it correctly
    -- but it was hard to get to after finishing.
    type equationElements(theType : charType) is record
        case theType is
            when Num =>
                number : Integer;
            when Sym =>
                symbol : Character;
        end case;
    end record;

    -- Defining the arrays I am going to use to collect input.
    type numberArray is Array(1 .. 100) of equationElements(Num);
    type symbolArray is Array(1 .. 100) of equationElements(Sym);

    -- Defining a subtype of Character, selecting only the mathematical symbols.
    subtype symbolType is Character with 
        Static_Predicate => symbolType in '*'|'/'|'+'|'-';

    -- Defining a new stack with symbolType as the itemType.
    package symbolStackPkg is new stackPkg(size => 100, itemType => symbolType);
    use symbolStackPkg;

    -- Defining a new stack with the itemType as Integer.
    package numberStackPkg is new stackPkg(size => 100, itemType => Integer);
    use numberStackPkg;

    -- Returns true if the top of the stack contains a operator, and false if
    -- the top contains a '(' character or if the stack is empty.
    function checkTop(symbolStack : in out symbolStackPkg.Stack) return boolean is
    begin
        if not isEmpty(symbolStack) then
            if top(symbolStack) /= '(' then
                return True;
            else
                return False;
            end if;
        else
            return False;
        end if;
    end checkTop;

    -- Checking in the next element in the array is a '(' character or not.
    function isIndexLeftParen(arr : in symbolArray; index : Integer) return Boolean is
    begin
            if arr(index).symbol = '(' then
                return True;
            else
                return False;
            end if;
    end isIndexLeftParen;

    -- The evaluate function accepts a numberStack and symbolStack as in out
    -- variables. Two numbers are popped off the integer stack and one symbol
    -- is popped off the symbol stack. These values are then calculated with
    -- the operator found at the top of the symbol stack.
    -- 
    -- Returns the answer of the equation.
    function evaluate(integerStack : in out numberStackPkg.Stack; 
        symbolStack : in out symbolStackPkg.Stack) return Integer is
        firstNumber : Integer := 0;
        secondNumber : Integer := 0;
        answer : Integer := 0;
        symbol : Character;
    begin
        secondNumber := top(integerStack);
        pop(integerStack);
        firstNumber := top(integerStack);
        pop(integerStack);
        symbol := top(symbolStack);
        pop(symbolStack);
        if symbol = '+' then
            answer := firstNumber + secondNumber;
        elsif symbol = '-' then
            answer := firstNumber - secondNumber;
        elsif symbol = '*' then
            answer := firstNumber * secondNumber;
        elsif symbol = '/' then
            answer := firstNumber / secondNumber;
        end if;
        return answer;
    end evaluate;

    -- Evaluates a equation using order of operations. The equation is
    -- passed in using two stacks, one containing the numbers, and the
    -- other containing the mathematical symbols.
    procedure orderOfOpp(numArr : numberArray; symArr : symbolArray) is
        orderOfOppNS : numberStackPkg.stack;
        orderOfOppSS : symbolStackPkg.stack;
        lastValueWasInt : Boolean := False;
        lastValueWasSym : Boolean := False;
        doneWithEquation : Boolean := False;
        leftParenAhead : Boolean := false;
        rightParenAhead : Boolean := False;
        numberIndex : Integer := 1;
        symbolIndex : Integer := 1;
        numberOffArr : Integer := 0;
        symbolOffArr : Character;
        numberOfIntsOnStack : Integer := 0;
        answer : Integer := 0;
        equation : Unbounded_String;
    begin
        if symArr(1).symbol = '(' then
            lastValueWasInt := True;
        end if;
        while not doneWithEquation loop

            -- Handling all numbers.
            if not lastValueWasInt then
                numberOffArr := numArr(numberIndex).number;
                push(numberOffArr, orderOfOppNS);
                put(numberOffArr, 1);
                numberIndex := numberIndex + 1;
                if not isEmpty(orderOfOppSS) then
                    if top(orderOfOppSS) = '+' or top(orderOfOppSS) = '-' then
                        if symArr(symbolIndex).symbol = '+' or
                            symArr(symbolIndex).symbol = '-' then
                            answer := evaluate(orderOfOppNS, orderOfOppSS);
                            push(answer, orderOfOppNS);
                        elsif symArr(symbolIndex).symbol = ')' then
                            put(')');
                            answer := evaluate(orderOfOppNS, orderOfOppSS);
                            push(answer, orderOfOppNS);
                            symbolIndex := symbolIndex + 1;
                            if top(orderOfOppSS) = '(' then
                                pop(orderOfOppSS);
                            end if;
                        end if;
                    elsif top(orderOfOppSS) = '*' or top(orderOfOppSS) = '/' then
                        if symArr(symbolIndex).symbol = '+' or
                            symArr(symbolIndex).symbol = '-' then
                            answer := evaluate(orderOfOppNS, orderOfOppSS);
                            push(answer, orderOfOppNS);
                        elsif symArr(symbolIndex).symbol = '*' or
                            symArr(symbolIndex).symbol = '/' then
                            answer := evaluate(orderOfOppNS, orderOfOppSS);
                            push(answer, orderOfOppNS);
                        elsif symArr(symbolIndex).symbol = ')' then
                            put(')');
                            answer := evaluate(orderOfOppNS, orderOfOppSS);
                            push(answer, orderOfOppNS);
                            symbolIndex := symbolIndex + 1;
                            if top(orderOfOppSS) = '(' then
                                pop(orderOfOppSS);
                            end if;
                        end if;
                    end if;
                end if;
                lastValueWasInt := True;
                lastValueWasSym := False;

            -- Handling all mathematical symbols as well as parenthesis.
            elsif not lastValueWasSym then
                symbolOffArr := symArr(symbolIndex).symbol;
                if symbolOffArr = '(' then
                    push(symbolOffArr, orderOfOppSS);
                    put(symbolOffArr);
                    symbolIndex := symbolIndex + 1;
                    lastValueWasInt := isIndexLeftParen(symArr, symbolIndex);
                    lastValueWasSym := not isIndexLeftParen(symArr, symbolIndex);
                elsif symbolOffArr = ')' then
                    symbolIndex := symbolIndex + 1;
                    put(symbolOffArr);
                    if top(orderOfOppSS) in symbolType then
                        answer := evaluate(orderOfOppNS, orderOfOppSS);
                        push(answer, orderOfOppNS);
                    end if;
                    if top(orderOfOppSS) = '(' then
                        pop(orderOfOppSS);
                    end if;
                elsif symbolOffArr in symbolType then
                    symbolIndex := symbolIndex + 1;
                    put(' ' & symbolOffArr & ' ');
                    if not isEmpty(orderOfOppSS) then
                        if symbolOffArr = '*' or symbolOffArr = '/' then
                            if top(orderOfOppSS) = '*' or
                                top(orderOfOppSS) = '/' then
                                answer := evaluate(orderOfOppNS, orderOfOppSS);
                                push(answer, orderOfOppNS);
                            end if;
                            push(symbolOffArr, orderOfOppSS);
                            lastValueWasInt := isIndexLeftParen(symArr, symbolIndex);
                            lastValueWasSym := not isIndexLeftParen(symArr, symbolIndex);
                        elsif symbolOffArr = '+' or symbolOffArr = '-' then
                            while checkTop(orderOfOppSS) loop
                                if top(orderOfOppSS) in symbolType then
                                    answer := evaluate(orderOfOppNS, orderOfOppSS);
                                    push(answer, orderOfOppNS);
                                end if;
                            end loop;
                            push(symbolOffArr, orderOfOppSS); 
                            lastValueWasInt := isIndexLeftParen(symArr, symbolIndex);
                            lastValueWasSym := not isIndexLeftParen(symArr, symbolIndex);
                        end if;
                    else
                        push(symbolOffArr, orderOfOppSS);
                        lastValueWasInt := isIndexLeftParen(symArr, symbolIndex);
                        lastValueWasSym := not isIndexLeftParen(symArr, symbolIndex);
                    end if;
                elsif symbolOffArr = '=' then
                    doneWithEquation := True;
                    put(" = ");
                    if isEmpty(orderOfOppSS) then
                        answer := top(orderOfOppNS);
                    else
                        while checkTop(orderOfOppSS) loop
                            answer := evaluate(orderOfOppNS, orderOfOppSS);
                            push(answer, orderOfOppNS);
                        end loop;
                    end if;
                else
                    push(symbolOffArr, orderOfOppSS);
                    symbolIndex := symbolIndex + 1;
                    lastValueWasInt := False;
                    lastValueWasSym := True;
                end if;
            end if;
        end loop; 
        put(answer, 1);
        new_line;
    end orderOfOpp;

    -- Evaluating an equation from the left to the right. Two arrays are passed
    -- in, one containing the numbers, and one containing the mathematical
    -- symbols.
    procedure leftToRight(numArr : numberArray; symArr : symbolArray) is
        leftToRightNS : numberStackPkg.stack;
        leftToRightSS : symbolStackPkg.stack;
        lastValueWasInt : Boolean := False;
        lastValueWasSym : Boolean := False;
        doneWithEquation : Boolean := False;
        leftParenAhead : Boolean := False;
        rightParenAhead : Boolean := False;
        numberIndex : Integer := 1;
        symbolIndex : Integer := 1;
        numberOffArr : Integer := 0;
        symbolOffArr : Character;
        numberOfIntsOnStack : Integer := 0;
        answer : Integer := 0;
    begin
        -- If the top of the symbol stack contains a '(' character, parenthesis
        -- appeared before a number in the equation.
        if symArr(1).symbol = '(' then
            lastValueWasInt := True;
            leftParenAhead := True;
        end if;
        while not doneWithEquation loop
            if not lastValueWasInt then
                numberOffArr := numArr(numberIndex).number;
                push(numberOffArr, leftToRightNS);
                put(numberOffArr, 1);
                numberOfIntsOnStack := numberOfIntsOnStack + 1;
                numberIndex := numberIndex + 1;

                -- Evaluating the numbers on the stack if the symbol stack is
                -- not empty and the integer stack contains at least 2 integers.
                if not isEmpty(leftToRightSS) then
                    if top(leftToRightSS) /= '(' then
                        if numberOfIntsOnStack >= 2 and symArr(symbolIndex).symbol /= '(' then
                            answer := evaluate(leftToRightNS, leftToRightSS);
                            push(answer, leftToRightNS);
                            numberOfIntsOnStack := numberOfIntsOnStack - 1;
                        end if;
                    end if;
                end if;
                lastValueWasInt := True;
                lastValueWasSym := False;
            elsif not lastValueWasSym then
                symbolOffArr := symArr(symbolIndex).symbol;
                if symbolOffArr = '(' then
                    push(symbolOffArr, leftToRightSS);
                    put(symbolOffArr);
                    symbolIndex := symbolIndex + 1;
                    lastValueWasInt := isIndexLeftParen(symArr, symbolIndex);
                    lastValueWasSym := not isIndexLeftParen(symArr, symbolIndex);
                elsif symbolOffArr = ')' then
                    while checkTop(leftToRightSS) loop
                        answer := evaluate(leftToRightNS, leftToRightSS);
                        push(answer, leftToRightNS);
                        numberOfIntsOnStack := numberOfIntsOnStack - 1;
                    end loop;
                    pop(leftToRightSS);
                    put(symbolOffArr);
                    symbolIndex := symbolIndex + 1;
                    if symArr(symbolIndex).symbol = ')' then
                        rightParenAhead := True;
                        lastValueWasInt := True;
                        lastValueWasSym := False;
                    else
                        lastValueWasInt := True;
                        lastValueWasSym := False;
                    end if;
                elsif symbolOffArr = '=' then
                    doneWithEquation := True;
                    while checkTop(leftToRightSS) loop
                        answer := evaluate(leftToRightNS, leftToRightSS);
                        push(answer, leftToRightNS);
                        numberOfIntsOnStack := numberOfIntsOnStack - 1;
                        end loop;
                    put(" = ");
                    symbolIndex := symbolIndex + 1;
                    if isEmpty(leftToRightSS) then
                        answer := top(leftToRightNS);
                    end if;
                else
                    while checkTop(leftToRightSS) loop
                        answer := evaluate(leftToRightNS, leftToRightSS);
                        push(answer, leftToRightNS);
                        numberOfIntsOnStack := numberOfIntsOnStack - 1;
                        end loop;
                    push(symbolOffArr, leftToRightSS);
                    put(' ' & symbolOffArr & ' ');
                    symbolIndex := symbolIndex + 1;
                    lastValueWasInt := isIndexLeftParen(symArr, symbolIndex);
                    lastValueWasSym := not isIndexLeftParen(symArr, symbolIndex);
                end if;
            end if;
        end loop; 
        put(answer, 1);
        new_line;
    end leftToRight;

    -- Fills two arrays with the equation. One with the integers and the
    -- other with the mathematical symbols.
    procedure fillArr is
        numArr : numberArray;
        symArr : symbolArray;
        eol : Boolean;
        nextChar : Character;
        numberCounter : Integer := 0;
        symbolCounter : Integer := 0;
        currentInt : Integer := 0;
        integerSeen : Boolean := false;
        equationSolved : Boolean := False;
    begin
        while not equationSolved loop
            look_Ahead(nextChar, eol);
            if eol then
                skip_line;
            elsif nextChar = '(' or nextChar = ')' then
                symbolCounter := symbolCounter + 1;
                symArr(symbolCounter).symbol := nextChar;
                get(nextChar);
            elsif Is_Digit(nextChar) or 
                (nextChar = '-' and not integerSeen) then
                Ada.Integer_Text_IO.get(currentInt);
                numberCounter := numberCounter + 1;
                numArr(numberCounter).number := currentInt;
                integerSeen := True;
            elsif nextChar in symbolType then
                symbolCounter := symbolCounter + 1;
                symArr(symbolCounter).symbol := nextChar;
                get(nextChar);
                integerSeen := False;
            elsif nextChar = '=' then
                equationSolved := True;
                symbolCounter := symbolCounter + 1;
                symArr(symbolCounter).symbol := nextChar;
            else
                get(nextChar);
            end if;
        end loop;
        leftToRight(numArr, symArr);
        orderOfOpp(numArr, symArr);
        new_line;
    end fillArr;

begin
    while not End_of_File loop
        fillArr;
        skip_line;
    end loop;
end opcalcs;

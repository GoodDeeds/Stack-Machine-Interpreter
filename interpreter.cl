(*
Program to simulate and interpret a Stack Machine
Sukrut Rao
CS15BTECH11036
*)

(*
    class for representing a Stack of strings
*)
class Stack inherits IO {

    (*
        to check if the stack is empty and return accordingly
        the current class deals with empty stacks
        return value: true
    *)
    isEmpty() : Bool {
        true
    };

    (*
        to push a value into the stack
        if the string is empty, then nothing is pushed
        parameters:
            input - the string to be pushed
        return value : the modified stack
    *)
    push(input : String) : Stack {
        if not input = ""
        then (new NonEmptyStack).init(input,self)
        else self
        fi
    };

    (*
        to pop the value at the top of the stack
        for the empty stack, the empty string is always returned
        return value : ""
    *)
    pop() : String {
        ""
    };

    (*
        to provide the string at the top of the stack
        for the empty stack, the empty string is always returned
        return value : ""
    *)
    head() : String {
        ""
    };

    (*
        to provide the rest of the stack except the top value
        for the empty stack, the stack itself is returned
        return value : self
    *)
    tail() : Stack {
        self
    };

    (*
        to display the stack
        for the empty stack, nothing is displayed
        return value : self
    *)
    display() : Stack {
        self
    };

};

(*
    to represent a non-empty node in a stack
*)
class NonEmptyStack inherits Stack {

    -- the value at the top of the stack
    topValue : String;

    -- the rest of the stack except the top value
    restOfStack : Stack;

    (*
        to provide the string at the top of the stack
        return value : topValue
    *)
    head() : String {
        topValue
    };

    (*
        to provide the rest of the stack except the top value
        return value : restOfStack
    *)
    tail() : Stack {
        restOfStack
    };

    (*
        to check if the stack is empty and return accordingly
        having any node from this class indicates that the stack is non-empty
        return value : false
    *)
    isEmpty() : Bool {
        false
    };

    (*
        to create a new node and insert it in the stack
        parameters:
            input - the value to be pushed
            stack - the existing stack
        return value: the modified stack
    *)
    init(input : String, stack : Stack) : Stack {
        {
            topValue <- input;
            restOfStack <- stack;
            self;
        }
    };

    (*
        to pop the value at the top of the stack
        return value : the current value of topValue
    *)
    pop() : String {
        let currentTop : String <- topValue in
        {
            topValue <- restOfStack.head();
            restOfStack <- restOfStack.tail();
            currentTop;
        }
    };

    (*
        to display the stack
        return value : self
    *)
    display() : Stack {
        {
            if not topValue = ""
            then out_string(topValue.concat("\n"))
            else false
            fi;
            restOfStack.display();
            self;
        }
    };

};

(*
    base class for all commands accepted by the stack
*)
class StackCommand inherits IO {

    -- to store whether inputs are still being accepted
    acceptingInputs : Bool <- true;

    (*
        to provide whether inputs are still being accepted
        return value : acceptingInputs
    *)
    acceptsInput() : Bool {
        acceptingInputs
    };

    (*
        to accept a string and recognize the type of command
        parameters:
            input - the command
        return value : the command recognized
    *)
    parseCommand(input : String) : StackCommand {
        {
            if input = "e"
            then (new EvaluateCommand)
            else if input = "d"
            then (new DisplayCommand)
            else if input = "x"
            then (new StopCommand)
            else (new PushCommand)
            fi fi fi;
        }
    };

    (*
        to convert a string into its equivalent integer
        parameters:
            input_string - the string representation of the integer
        return value : the integer corresponding to input_value
    *)
    atoi(input_string : String) : Int {
		let result : Int <- 0, i : Int <- 0 in
		{
			while i < input_string.length()
			loop
			{
				result <- result * 10 + convInt(input_string.substr(i,1));
				i <- i + 1;
			}
			pool;
			result;
		}
	};

    (*
        to convert a string representation of a digit into its equivalent integer
        incorrect input gives 0
        parameters:
            input - the string representation of the digit
        return value : the integer equivalent of the string
    *)
    convInt(input : String) : Int {
		let result : Int <- 0 in
		{
			if input = "1" then result <- 1 else
			if input = "2" then result <- 2 else
			if input = "3" then result <- 3 else
			if input = "4" then result <- 4 else
			if input = "5" then result <- 5 else
			if input = "6" then result <- 6 else
			if input = "7" then result <- 7 else
			if input = "8" then result <- 8 else
			if input = "9" then result <- 9 else
			result <- 0
			fi fi fi fi fi fi fi fi fi;
			result;
		}
	};

    (*
        to compute the modulo of a positive integer with respect to another positive integer
        parameters:
            a - the integer whose modulo is to be taken
            b - the integer with respect to which the modulo is to be taken
        return value : a modulo b
    *)
    modulo(a : Int, b : Int) : Int {
		let q : Int, r : Int in
		{
			if b = 0
			then 0
			else
			{
				q <- a / b;
				r <- a - b * q;
				r;
			}
			fi;
		}
	};

    (*
        to convert an integer into its equivalent string
        parameters:
            input - the integer
        return value : the string corresponding to input
    *)
    itoa(input : Int) : String {
		let result : String <- "", r : Int in
		{
			if input = 0
			then result <- "0"
			else
			{
				while 0 < input
				loop
				{
					r <- modulo(input,10);
					result <- convString(r).concat(result);
					input <- input / 10;
				}
				pool;
			}
			fi;
			result;
		}
	};

    (*
        to convert an integer digit into its equivalent string
        incorrect input gives "0"
        parameters:
            input - the integer digit
        return value : the string equivalent of input
    *)
    convString(input : Int) : String {
		let result : String in
		{
			if input = 1 then result <- "1" else
			if input = 2 then result <- "2" else
			if input = 3 then result <- "3" else
			if input = 4 then result <- "4" else
			if input = 5 then result <- "5" else
			if input = 6 then result <- "6" else
			if input = 7 then result <- "7" else
			if input = 8 then result <- "8" else
			if input = 9 then result <- "9" else
			result <- "0"
			fi fi fi fi fi fi fi fi fi;
			result;
		}
	};

    (*
        to evaluate a given command
        this function is meant to be used in inherited classes when redefined
        paramters:
            stack - the current stack
            input - the command
        return value : the current stack
    *)
    evaluateCommand(stack : Stack, input : String) : Stack {
        stack
    };
};

(*
    class for commands involving pushing a string to the stack
*)
class PushCommand inherits StackCommand {

    (*
        to evaluate a given command
        here, it evaluates the push command by pushing the input to the stack
        parameters:
            stack - the current stack
            input - the command
        return value : the modified stack
    *)
    evaluateCommand(stack : Stack, input : String) : Stack {
        stack.push(input)
    };
};

(*
    class for the command to display the stack
*)
class DisplayCommand inherits StackCommand {

    (*
        to evaluate a given command
        here, it evaluates the display command by displaying the stack
        parameters:
            stack - the current stack
            input - the command
        return value : the current stack
    *)
    evaluateCommand(stack : Stack, input : String) : Stack {
            stack.display();
    };
};

(*
    class for the stop command to stop the program
*)
class StopCommand inherits StackCommand {

    (*
        to evaluate a given command
        here, it evaluates the stop command by setting acceptingInputs to false
        parameters:
            stack - the current stack
            input - the command
        return value : the current stack
    *)
    evaluateCommand(stack : Stack, input : String) : Stack {
        {
            acceptingInputs <- false;
            stack;
        }
    };
};

(*
    class for the evaluate command to evaluate based on the value at the top
    of the stack
*)
class EvaluateCommand inherits StackCommand {

    (*
        to evaluate a given command
        here, it evaluates the evaluate command by appropriately performing
        operations based on the top element of the stack
        parameters:
            stack - the current stack
            input - the command
        return value : the modified stack
    *)
    evaluateCommand(stack : Stack, input : String) : Stack {
        let topValue : String, oneFromTop : String, twoFromTop : String in
        {
            topValue <- stack.pop();
            oneFromTop <- stack.pop();
            twoFromTop <- stack.pop();
            if topValue = "+"
            then stack.push(itoa(atoi(oneFromTop) + atoi(twoFromTop)))
            else if topValue = "s"
            then stack.push(oneFromTop).push(twoFromTop)
            else stack.push(twoFromTop).push(oneFromTop).push(topValue)
            fi fi;
        }
    };
};

(*
    the Main class
*)
class Main inherits IO {

    -- to parse input and provide the current command
    stackCommand : StackCommand <- new StackCommand;

    -- to store the current command and to evaluate it
    currentStackCommand : StackCommand <- new StackCommand;

    -- to represent the stack
    stack : Stack <- new Stack;

    -- to store the input command
    input : String;

    (*
        the main function
    *)
    main() : Object {
        while currentStackCommand.acceptsInput()
        loop
        {
            out_string(">");
            input <- in_string();
            currentStackCommand <- stackCommand.parseCommand(input);
            stack <- currentStackCommand.evaluateCommand(stack,input);
        }
        pool
    };
};

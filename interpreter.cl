class Stack inherits IO {

    isEmpty() : Bool {
        true
    };

    push(input : String) : Stack {
        if not input = ""
        then (new NonEmptyStack).init(input,self)
        else self
        fi
    };

    pop() : String {
        ""
    };

    head() : String {
        ""
    };

    tail() : Stack {
        self
    };

    display() : Stack {
        self
    };

};

class NonEmptyStack inherits Stack {

    topValue : String;

    restOfStack : Stack;

    head() : String {
        topValue
    };

    tail() : Stack {
        restOfStack
    };

    isEmpty() : Bool {
        false
    };

    init(input : String, stack : Stack) : Stack {
        {
            topValue <- input;
            restOfStack <- stack;
            self;
        }
    };

    pop() : String {
        let currentTop : String <- topValue in
        {
            topValue <- restOfStack.head();
            restOfStack <- restOfStack.tail();
            currentTop;
        }
    };

    display() : Stack {
        {
            if not topValue = ""
            then out_string(topValue.concat("\n"))
            else false
            fi;
            restOfStack.display();
        }
    };

};

class StackCommand inherits IO {

    acceptingInputs : Bool <- true;

    acceptsInput() : Bool {
        acceptingInputs
    };

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

    evaluateCommand(stack : Stack, input : String) : Stack {
        stack
    };
};

class PushCommand inherits StackCommand {

    evaluateCommand(stack : Stack, input : String) : Stack {
        stack.push(input)
    };
};

class DisplayCommand inherits StackCommand {

    evaluateCommand(stack : Stack, input : String) : Stack {
        let oldStack : Stack <- stack in
        {
            stack.display();
            oldStack;
        }
    };
};

class StopCommand inherits StackCommand {

    evaluateCommand(stack : Stack, input : String) : Stack {
        {
            acceptingInputs <- false;
            stack;
        }
    };
};

class EvaluateCommand inherits StackCommand {

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

class Main inherits IO {

    stackCommand : StackCommand <- new StackCommand;

    currentStackCommand : StackCommand <- new StackCommand;

    stack : Stack <- new Stack;

    input : String;

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

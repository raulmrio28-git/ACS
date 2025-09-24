# ACS

A.C. Script format launcher source

This format was mainly done to replace batch file execution from the network for a network-specific task (e.g. BIOS Updates).

This format attempts to combine C and Python in a single format, executed without compilation or stack-based VM, inspired by a competitor's format that is based on Windows Batch script format.

# Some usage notes

## 1. This script supports script-wide value definition, for example a string:

```
String acs = "TestACS"
```

## 2. The function definition and comment syntax is similar to Python, altho body is done in C way:

```
Fun AnotherFun(None) -> None
{
	# your code here...
}
```

## 3. Supports built in functions (printing, executing, getting system variables):

1. Print example

```
Print("Inside AnotherFun")
```

2. Print example with parameters (like printf)

```
Print("Hello ACS %s %d %f %b", acs, num, 1.129420, True)
```

## 4. Nesting is somewhat supported:

```
{
    String nested = "Nested variable"
    Print("Inside nested block: %s", nested)
}
```

## 5. If/Else/Elseif is supported, so do for and while

```
for(i = 0 to 5; 1) {
    if(i == 1)
    {
        Print("i is one")
    }
    elseif(i == 2)
    {
        Print("i is two")
    }
    else
    {
        Print("i is something else: %d", i)
    }
}

Number j = 0
while(j < 3) {
    Print("While loop iteration: %d", j)
    j = j + 1
}
```

### 6. Can return various types in function

# Notes

Read the ACS format.docx files for rules and what's supported and what's planned to support.


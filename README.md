# A Domain Specific Language on Financial Contracts

This project is a part of my bachelor thesis at the bachelor programmee in Computer Science & Economics at University of Copenhagen. Grade received was 12 (A).

## Quick overview 
In the past decades, the financial derivatives market has experienced a rapid change,
resulting in a necessity to not only manage and valuate existing derivatives, but also to minimise time to market for new, exotic derivatives. In this thesis, we establish a framework
capable of expressing, managing and pricing a wide range of financial instruments. We do
this by constructing a DSL deeply embedded in F # which employs a set of constructors
grounded with logical principles, enabling the user to express a wide variety of financial
contracts. We supply with functions that allow for management of contracts expressed
within the language, and employ methods from continuous time finance for pricing.

In the file 'presentation.pdf', you can find a brief explanation on what this project is about.

In the file 'thesis.pdf', you can find the actual report that was handed in, which describes everything about the program.

In the folder 'code', you can find all the source code for the project. There are two projects in this folder. The main project and solution, BachelorThesis, which can be found in the src folder, contains implementation. The Tests folder contains tests of the implementation.

## Running the results of the report
To run the results of the report, go into the root folder in the command prompt and run the command
```
dotnet build
```
  
followed by

```
dotnet run
```
  
To run only the results for section 5, run the command
```
dotnet run section5
```
  
To run only the results for section 7, run the command

```
dotnet run section7
  ```
  
## Running the tests from section 6
To run the tests, go into the Tests folder. Build the project and run the command

```
dotnet test
```
output:
```
- Passed!  - Failed:     0, Passed:   175, Skipped:     0, Total:   175, Duration: 1 m 19 s - Tests.dll (net7.0)
```
To run only the test example of the square function, run the command
```
dotnet test --filter testProgram
```
output:
```
- Passed!  - Failed:     0, Passed:     3, Skipped:     0, Total:     3, Duration: 6 ms - Tests.dll (net7.0)
```

To run only the contract management tests, run the command
```
  dotnet test --filter ContractManagement
```
output:
```
- Passed!  - Failed:     0, Passed:    43, Skipped:     0, Total:    43, Duration: 115 ms - Tests.dll (net7.0)
```
To run the pricing tests, run the following commands:
```
  dotnet test --filter currencyTests
  dotnet test --filter observableTests
  dotnet test --filter contractTests
  dotnet test --filter simulationTests
```
outputs:
```
- Passed!  - Failed:     0, Passed:     8, Skipped:     0, Total:     8, Duration: 8 ms - Tests.dll (net7.0)
- Passed!  - Failed:     0, Passed:    68, Skipped:     0, Total:    68, Duration: 55 ms - Tests.dll (net7.0)
- Passed!  - Failed:     0, Passed:    40, Skipped:     0, Total:    40, Duration: 16 s - Tests.dll (net7.0)
- Passed!  - Failed:     0, Passed:    13, Skipped:     0, Total:    13, Duration: 57 s - Tests.dll (net7.0)
```
